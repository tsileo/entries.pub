open Lwt.Infix
open Lwt
open Yurt
include Cohttp_lwt_unix.Server

open Config
open Entry

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
    let current = Ezjsonm.(get_strings (find current_data ["properties"; k])) in
    let new_v =
      current @ Ezjsonm.(get_strings v)
      |> List.map (fun x -> `String x)
     in
     let nl = List.remove_assoc k acc in
     nl @ [k, `A new_v]
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
    let slug = slugify (jform_field (`O last_doc) ["properties"; "name"] "") in
    if slug = "" then failwith "no slug" else
    let js = Ezjsonm.(to_string (`O last_doc)) in
    Store.set t ~info:(info "Updating an entry") ["entries"; slug] js >>= fun () ->
      let headers = Header.init ()
      |> fun h -> Header.add h "Location" (base_url ^ "/" ^ slug)
      in
      Server.json ~status:201 ~headers (`O last_doc)
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
           |> fun h -> Header.add h "Location" (base_url ^ "/" ^ slug) in
          Server.string "" ~status:201 ~headers


let is_multipart_regexp = Str.regexp "multipart/.*"
let is_form content_type : bool =
  let is_multipart = Str.string_match (is_multipart_regexp) content_type 0 in
  if is_multipart || content_type = "application/x-www-form-urlencoded" then
    true
  else
    false

let head (r : string) (ep : Yurt.endpoint) (s : Server.server) =
  Server.register_route_string s "HEAD" r ep


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
    ) keys >>= fun entries ->
    let updated = if List.length entries > 0 then
        let last_one = List.hd entries in
        Ezjsonm.(get_string (find last_one ["published"]))
    else "" in
    let dat = `O [
      "websub_endpoint", `String websub_endpoint;
      "name", `String blog_name;
      "base_url", `String base_url;
      "updated", `String updated;
      "entries", `A entries;
    ] in
    let out = Mustache.render atom_tpl dat in
    let headers = Header.init ()
      |> fun h -> Header.add h "Content-Type" "application/xml"
      |> fun h -> Header.add h "Link" "</atom.xml>; rel=\"self\""
      |> fun h -> Header.add h "Link" ("<" ^ websub_endpoint ^ ">; rel=\"hub\"") in
    string out ~headers)

(* Index *)
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

(* Micropub endpoint *)
>| post "/webmention" (fun req params body ->
  Webmention.discover_webmention "http://google.com" >>= fun res ->
  let v = Yurt_util.unwrap_option_default res "" in
  string v)
  (*
  Websub.ping "https://google.com" >>= fun res ->
  if res then string "yes" else string "no")
  *)

(* Handle Micropub queries *)
>| get "/micropub" (fun req params body ->
  (* TODO check auth and handle JSON *)
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

(* Micropub endpoint *)
>| post "/micropub" (fun req params body ->
    (* TODO check auth and handle JSON *)
    let content_type = Yurt_util.unwrap_option_default (Header.get req.Request.headers "Content-Type") "" in
    if content_type = "application/json" then
      handle_json_create body
    else
      handle_form_create body)

(* HEAD index *)
>| head "/" (fun req params body ->
  let headers = Header.init ()
   |> add_headers in
   string "" ~headers)
 
(* HEAD Post/entry page *)
>| head "/<slug:string>" (fun req params body ->
  let slug = Route.string params "slug" in
   Store.Repo.v config >>=
   Store.master >>= fun t ->
     Store.find t ["entries"; slug] >>= fun some_stored ->
       match some_stored with
       | Some stored ->
		let headers = Header.init ()
         |> add_headers in
         string "" ~headers
       | None ->
        (* 404 *)
		let headers = Header.init ()
         |> add_headers in
         string "" ~headers ~status:404)
 
(* Post/entry page *)
>| get "/<slug:string>" (fun req params body ->
  let slug = Route.string params "slug" in
   Store.Repo.v config >>=
   Store.master >>= fun t ->
     Store.find t ["entries"; slug] >>= fun some_stored ->
       match some_stored with
       | Some stored ->
         (* Render the entry *)
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
         (* Return a 404 *)
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
