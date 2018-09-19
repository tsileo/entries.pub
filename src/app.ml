open Lwt.Infix
open Lwt
open Yurt
include Cohttp_lwt_unix.Server

open Config
open Entry
open Micropub
open Microformats

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
  Microformats.test_mf2 >>= fun out ->
  json out)
  (*
  Webmention.discover_webmention "http://google.com" >>= fun res ->
  let v = Yurt_util.unwrap_option_default res "" in
  string v)
  Websub.ping "https://google.com" >>= fun res ->
  if res then string "yes" else string "no")
  *)

(* Handle Micropub queries *)
>| get "/micropub" (fun req params body ->
 micropub_query req)

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
