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

let tpl = load_file "template.html"
let atom_tpl = load_file "atom.xml"

(* TODO uses Ezjsonm.find instead *)
let find l key =
  match l with
  | `O list -> List.assoc key list
  | _ -> assert false

(* Micropub API error *)
let invalid_request_error desc = 
  `O [ "error",  `String "invalid_request"; "error_description", `String desc]

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

(* Create a new note via a POSTed form *)
let handle_form_create body =
  Form.urlencoded_json body >>= fun p ->
    let jdata = Ezjsonm.(value p) in
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
      Store.get t ["entries";s] >>= fun stored ->
        stored |> Ezjsonm.from_string |> entry_tpl_data |> Lwt.return
    ) keys >>= fun dat ->
    let mtpl = Mustache.of_string atom_tpl in
    (* TODO sort data by published *)
    let dat = `O [
      "name", `String "Name";
      "base_url", `String base_url;
      "updated", `String "2015-07-21T18:01:00+02:00";
      "entries", `A dat;
    ] in
    let out = Mustache.render mtpl dat in
    let headers = Header.init ()
      |> fun h -> Header.add h "Content-Type" "application/xml" in
    string out)

>| get "/" (fun req params body ->
  Store.Repo.v config >>=
  Store.master >>= fun t ->
  Store.list t ["entries"] >>= fun keys ->
    Lwt_list.map_s (fun (s, c) ->
      Store.get t ["entries";s] >>= fun stored ->
        stored |> Ezjsonm.from_string |> entry_tpl_data |> Lwt.return
    ) keys >>= fun dat ->
    let mtpl = Mustache.of_string tpl in
    let dat = `O [
      "entries", `A (List.sort compare_entry_data dat);
      "base_url", `String base_url;
      "is_index", `Bool true;
      "is_entry", `Bool false;
    ] in
    let out = Mustache.render mtpl dat in
    let headers = Header.init ()
      |> add_headers in
    string out ~headers)

>| get "/<slug:string>" (fun req params body ->
  let slug = Route.string params "slug" in
   Store.Repo.v config >>=
   Store.master >>= fun t ->
    Store.get t ["entries";slug] >>= fun stored ->
        let nstored = stored |> Ezjsonm.from_string |> entry_tpl_data in
        let mtpl = Mustache.of_string tpl in
        let dat = `O [
          "is_index", `Bool false;
          "is_entry", `Bool true;
          "base_url", `String base_url;
          "entry", nstored;
        ] in
        let out = Mustache.render mtpl dat in
        let headers = Header.init ()
          |> add_headers in
        string out ~headers)

>| get "/lol" (fun req params body ->
  let dat1 = Omd.of_string "**hey**" |> Omd.to_html  in
  let mtpl = Mustache.of_string tpl in
  let dat = `O ["name", `String "thomas";"hey", `String dat1] in
  let out = Mustache.render mtpl dat in
  let headers = Header.init ()
    |> add_headers in
  string out ~headers)

>| post "/micropub" (fun req params body->
    (* TODO check auth and handle JSON *)
    handle_form_create body)

(* Run it *)
|> run
