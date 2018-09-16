open Lwt.Infix
open Lwt
open Yurt
include Cohttp_lwt_unix.Server

module Store = Irmin_unix.Git.FS.KV(Irmin.Contents.String)

module Date = struct
  module D = ODate.Unix
  (* iso format *)
  let format = "%FT%TZ"
  let parseR = match D.From.generate_parser format with
    | Some p -> p
    | None -> failwith "could not generate parser"
  let printer = match D.To.generate_printer format with
    | Some p -> p
    | None -> failwith "could not generate printer"
  let pformat = "%b %d, %Y"
  let pprinter = match D.To.generate_printer pformat with
    | Some p -> p
    | None -> failwith "could not generate pprinter"


  type t = D.t
  let now () = D.now ()
  let to_string d = D.To.string ~tz:ODate.UTC printer d
  let to_pretty d = D.To.string ~tz:ODate.UTC pprinter d
  let of_string s = D.From.string parseR s
end

(* Slugify replaces whitespaces by dashes, lowecases and remove any non alphanum chars. *)
let slugify k =
  k
  |> Str.global_replace (Str.regexp " ") "-"
  |> String.lowercase_ascii
  |> Str.global_replace (Str.regexp "[^a-z0-9\\-]") ""

(* Irmin config *)
let config = Irmin_git.config ~bare:false "./db"

let author = "entries.pub <dev@entries.pub>"
 
let info fmt = Irmin_unix.info ~author fmt

(* Read the file as a string *)
let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = String.create n in
  really_input ic s 0 n;
  close_in ic;
  (s)

let html_tpl =
  load_file "template.html"
  |> Mustache.of_string

let atom_tpl =
  load_file "atom.xml"
  |> Mustache.of_string

(* TODO uses Ezjsonm.find instead *)
let find l key =
  match l with
  | `O list -> List.assoc key list
  | _ -> assert false

(* Micropub API error *)
let invalid_request_error desc = 
  `O [
    "error",  `String "invalid_request";
    "error_description", `String desc;
  ]

(* Return the first item of the given list or a data *)
let jform_field jdata k default =
  if Ezjsonm.(mem jdata k) then
    let items = Ezjsonm.(get_strings (find jdata k)) in
      match items with
        | [] -> default
        | t :: _ -> t
  else
    default

(* Return the first item of the given list or a data *)
let jdata_field jdata k default =
  if Ezjsonm.(mem jdata k) then
    Ezjsonm.(get_string (find jdata k))
  else
    default

let rconf = match (load_file "config.yaml" |> Yaml.of_string) with
  | Ok r -> r
  | Error e -> failwith "failed to load config"

let base_url = jdata_field rconf ["base_url"] "http://localhost:7888"
let author_name = jdata_field rconf ["author_name"] "Dev"
let author_email = jdata_field rconf ["author_email"] "dev@entries.pub"

let compare_entry_data a b =
  let a_published = jdata_field a ["published"] "" in
  let b_published = jdata_field b ["published"] "" in
  (String.compare b_published a_published)

(* Convert a JSON entry into template data *)
let entry_tpl_data jdata =
  let name = jform_field jdata ["properties"; "name"] "" in
  let content = jform_field jdata ["properties"; "content"] "" in
  let published = jform_field jdata ["properties"; "published"] "" in
  let slug = slugify name in
  `O [
    "name", `String name;
    "slug", `String slug;
    "content", `String (Omd.of_string content |> Omd.to_html);
    "published", `String published;
    "published_pretty", `String (Date.of_string published |> Date.to_pretty);
    "author_name", `String author_name;
    "author_email", `String author_email;
    "url", `String (base_url ^ "/" ^ slug);
  ]

let path_to_slug str =
  if str = "" then "" else
  String.sub str 1 ((String.length str) - 1)

(* Micropub delete action handler *)
let micropub_delete url =
  if url == "" then
    Server.json (invalid_request_error "url") ~status:400
  else
  let slug = Uri.of_string url |> Uri.path |> path_to_slug in
  Store.Repo.v config >>=
  Store.master >>= fun t ->
  Store.find t ["entries"; slug] >>= fun some_stored ->
  match some_stored with
  | Some stored ->
    Store.remove t ~info:(info "Deleting an entry") ["entries"; slug] >>= fun () ->
      Server.string "" ~status:204
  | None -> Server.json (`O ["error", `String "url not found"]) ~status:404


let micropub_update_delete current_data jdata =
  if not Ezjsonm.(mem jdata ["delete"]) then Ezjsonm.(get_dict current_data) else
  let props = Ezjsonm.(get_dict (find current_data ["properties"])) in
  let deleted = Ezjsonm.(find jdata ["delete"]) in
  let new_props = match deleted with
  | `O d ->
    List.fold_left (fun acc x ->
      let (k, v) = x in
      let current = Ezjsonm.(get_strings (find current_data ["properties"; k])) in
      let to_delete = Ezjsonm.(get_strings v) in
      let new_data =
        current
        |> List.filter (fun x -> not (List.mem x to_delete))
        |> List.map (fun x -> `String x)
      in
      let nl = List.remove_assoc k acc in
      if List.length new_data > 0 then
        nl @ [k, (`A new_data)]
      else
        nl
    ) props d
  | `A ks ->
    List.fold_left (fun acc x ->
      List.remove_assoc x acc
    ) props Ezjsonm.(get_strings deleted)
  | _ -> failwith "lol" in
  Ezjsonm.(get_dict current_data)
  |> List.remove_assoc "properties"
  |> List.append ["properties", `O new_props]


let micropub_update_add current_data jdata =
  if not Ezjsonm.(mem jdata ["add"]) then Ezjsonm.(get_dict current_data) else
  let props = Ezjsonm.(get_dict (find current_data ["properties"])) in
  let added = Ezjsonm.(get_dict (find jdata ["add"])) in
  let new_props = List.fold_left (fun acc (k, v) ->
    if not Ezjsonm.(mem current_data ["properties"; k]) then
      acc @ [k, v]
    else
    (* TODO a mem and empty list *)
    let current = Ezjsonm.(get_strings (find current_data ["properties"; k])) in
    let new_v =
      current @ Ezjsonm.(get_strings v)
      |> List.map (fun x -> `String x)
     in
     let nl = List.remove_assoc k acc in
     nl @ [k, `A new_v]  (* FIXME append values *)
  ) props added in
  Ezjsonm.(get_dict current_data)
  |> List.remove_assoc "properties"
  |> List.append ["properties", `O new_props]


let micropub_update_replace current_data jdata =
  if not Ezjsonm.(mem jdata ["replace"]) then Ezjsonm.(get_dict current_data) else
  let props = Ezjsonm.(get_dict (find current_data ["properties"])) in
  let replaced = Ezjsonm.(get_dict (find jdata ["replace"])) in
  let new_props = List.fold_left (fun acc (k, v) ->
    let nl = List.remove_assoc k acc in
    nl @ [k, v]
  ) props replaced in
  Ezjsonm.(get_dict current_data)
  |> List.remove_assoc "properties"
  |> List.append ["properties", `O new_props]


let micropub_update url jdata =
  if url = "" then Server.json (invalid_request_error "url") ~status:400 else
  let slug = Uri.of_string url |> Uri.path |> path_to_slug in
  Store.Repo.v config >>=
  Store.master >>= fun t ->
  Store.find t ["entries"; slug] >>= fun some_stored ->
  match some_stored with
  | Some stored ->
    let current_data = Ezjsonm.(from_string stored) in
    let doc_with_replace = micropub_update_replace current_data jdata in
    let doc_with_delete = micropub_update_delete (`O doc_with_replace) jdata in
    let last_doc = micropub_update_add (`O doc_with_delete) jdata in
    (* TODO handle the update *)
    Server.json (`O last_doc)
  | None -> Server.json (`O ["error", `String "url not found"]) ~status:404


(* Micropub JSON handler *)
let handle_json_create body =
  Body.to_string body >>= fun sbody ->
    let jdata = Ezjsonm.from_string sbody in
    (* Handle actions *)
    let action = jdata_field jdata ["action"] "" in
    let url = jdata_field jdata ["url"] "" in
    if action = "delete" then micropub_delete url else
    if action = "update" then micropub_update url jdata else
    (* Continue to process the entry creation *)
    let entry_type = jform_field jdata ["type"] "" in
    (* TODO handle entry creation *)
    Server.string entry_type


(* Micropub form handler *)
let handle_form_create body =
  Form.urlencoded_json body >>= fun p ->
    let jdata = Ezjsonm.(value p) in
    (* Handle actions *)
    let action = jform_field jdata ["action"] "" in
    let url = jform_field jdata ["url"] "" in
    if action = "delete" then micropub_delete url else
    (* Continue to process the entry creation *)
    let entry_type = jform_field jdata ["h"] "entry" in
    let entry_content = jform_field jdata ["content"] "" in
    let entry_name = jform_field jdata ["name"] "" in
    let entry_published = jform_field jdata ["published"] (Date.now () |> Date.to_string) in
    if entry_content = "" then
      Server.json (invalid_request_error "missing content") ~status:400
    else if entry_name = "" then
      Server.json (invalid_request_error "missing name") ~status:400
    else if entry_published = "" then
      Server.json (invalid_request_error "invalid published") ~status:400
    else if entry_type <> "entry" then
      Server.json (invalid_request_error "invalid type, only entry is supported") ~status:400
    else
      let obj = `O [
        "type", `A [ `String ("h-" ^ entry_type) ];
        "properties", `O [
          "content", `A [ `String entry_content ];
          "name", `A [ `String entry_name ];
          "published", `A [ `String entry_published ];
        ]
      ] in
      let slug = slugify entry_name in
      let js = Ezjsonm.to_string obj in
      Store.Repo.v config >>=
      Store.master >>= fun t ->
        Store.set t ~info:(info "Creating a new entry") ["entries"; slug] js >>= fun () ->
          let headers = Header.init ()
          (* TODO fix the Location URL *)
           |> fun h -> Header.add h "Location" "http://entries.pub/" in
          Server.string "" ~status:201 ~headers

let add_headers h =
   h
   |> fun h -> Header.add h "Content-Type" "text/html; charset=utf-8"
   |> fun h -> Header.add h "X-Powered-By" "entries.pub"
   |> fun h -> Header.add h "Link" ("<" ^ base_url ^ "/micropub>; rel=\"micropub\"")


let is_multipart_regexp = Str.regexp "multipart/.*"
let is_form content_type : bool =
  let is_multipart = Str.string_match (is_multipart_regexp) content_type 0 in
  if is_multipart || content_type = "application/x-www-form-urlencoded" then
    true
  else
    false


(* Create a server *)
let _ =
let open Server in

server "127.0.0.1" 7888


(* Atom feed *)
>| get "/atom.xml" (fun req params body ->
  Store.Repo.v config >>=
  Store.master >>= fun t ->
  Store.list t ["entries"] >>= fun keys ->
    Lwt_list.map_s (fun (s, c) ->
      Store.get t ["entries"; s] >>= fun stored ->
        stored |> Ezjsonm.from_string |> entry_tpl_data |> Lwt.return
    ) keys >>= fun dat ->
    let dat = `O [
      "name", `String "Name";
      "base_url", `String base_url;
      (* TODO List.tl *)
      "updated", `String "2015-07-21T18:01:00+02:00";
      "entries", `A dat;
    ] in
    let out = Mustache.render atom_tpl dat in
    let headers = Header.init ()
      |> fun h -> Header.add h "Content-Type" "application/xml"
      |> fun h -> Header.add h "Link" "</atom.xml>; rel=\"self\""
      |> fun h -> Header.add h "Link" "<https://pubsubhubbub.superfeedr.com/>; rel=\"hub\"" in
    string out ~headers)


>| get "/" (fun req params body ->
  Store.Repo.v config >>=
  Store.master >>= fun t ->
  Store.list t ["entries"] >>= fun keys ->
    Lwt_list.map_s (fun (s, c) ->
      Store.get t ["entries"; s] >>= fun stored ->
        stored |> Ezjsonm.from_string |> entry_tpl_data |> Lwt.return
    ) keys >>= fun dat ->
    let dat = `O [
      "entries", `A (List.sort compare_entry_data dat);
      "base_url", `String base_url;
      "is_index", `Bool true;
      "is_entry", `Bool false;
      "is_404", `Bool false;
    ] in
    let out = Mustache.render html_tpl dat in
    let headers = Header.init ()
      |> add_headers in
    string out ~headers)


>| get "/micropub" (fun req params body ->
  (* Handle queries *)
  let q = Yurt_util.unwrap_option_default (Query.string req "q") "" in
  let url = Yurt_util.unwrap_option_default (Query.string req "url") "" in
  if q = "config" then Server.json (`O []) else
  if q = "syndicate-to" then Server.json (`O ["syndicate-to", `A []]) else
  if q = "source" then begin
  if url = "" then
    (json (invalid_request_error "url") ~status:400)
  else
  let slug = Uri.of_string url |> Uri.path |> path_to_slug in
  Store.Repo.v config >>=
  Store.master >>= fun t ->
  Store.find t ["entries"; slug] >>= fun some_stored ->
    match some_stored with
    | Some stored ->
      let current_data = Ezjsonm.(from_string stored) in
      json current_data
    | None ->
      json (`O ["error", `String "url not found"]) ~status:404
  end else
  json (`O []))


>| post "/micropub" (fun req params body ->
    let content_type = Yurt_util.unwrap_option_default (Header.get req.Request.headers "Content-Type") "" in
    if content_type = "application/json" then
    (* TODO check auth and handle JSON *)
      handle_json_create body
    else
      handle_form_create body)
    (* string "bad content-type" ~status:415) *)


>| get "/<slug:string>" (fun req params body ->
  let slug = Route.string params "slug" in
   Store.Repo.v config >>=
   Store.master >>= fun t ->
      Store.find t ["entries"; slug] >>= fun some_stored ->
        match some_stored with
        | Some stored ->
          let nstored = stored |> Ezjsonm.from_string |> entry_tpl_data in
          let dat = `O [
            "is_index", `Bool false;
            "is_entry", `Bool true;
            "is_404", `Bool false;
            "base_url", `String base_url;
            "entry", nstored;
          ] in
          let out = Mustache.render html_tpl dat in
          let headers = Header.init ()
            |> add_headers in
          (string out ~headers)
        | None ->
          let dat = `O [
            "is_index", `Bool false;
            "is_entry", `Bool false;
            "is_404", `Bool true;
            "base_url", `String base_url;
          ] in
          let out = Mustache.render html_tpl dat in
          let headers = Header.init ()
            |> add_headers in
           string out ~headers ~status:404)

(* Run it *)
|> run
