open Lwt.Infix
open Lwt
open Yurt
open Printf
include Cohttp_lwt_unix.Server
include Cohttp

open Entriespub
open Entriespub.Entry
open Entriespub.Micropub
open Entriespub.Utils
open Entriespub.Config

let is_multipart_regexp = Str.regexp "multipart/.*"
let is_form content_type =
  let is_multipart = Str.string_match (is_multipart_regexp) content_type 0 in
  if is_multipart || content_type = "application/x-www-form-urlencoded" then
    true
  else
    false

(* Implement missing head helper *)
let head (r : string) (ep : Yurt.endpoint) (s : Server.server) =
  Server.register_route_string s "HEAD" r ep

(* Output a JSON error *)
let json_error code msg status =
  let headers = Header.init ()
  |> set_content_type "application/json" in

  Server.json (build_error code msg) ~status ~headers

(* Call the token endpoint in order to verify the token validity *)
let check_auth req =
  (* Get the bearer token from the incoming request *)
  let auth = Yurt_util.unwrap_option_default (Header.get req.Request.headers "Authorization") "" in
  if auth = "" then
     Lwt.return false
  else
    let headers =
      Header.init ()
      |> fun h -> Header.add h "Authorization" auth in

    (* And forward it to the token endpoint *)
    Client.get ~headers token_endpoint >>= fun (resp, body) ->
    match resp with
    | { Response.status = `OK } -> Lwt.return true
    | _ -> Lwt.return false

(* Log HTTP request to stdout *)
let log_req req =
  let meth = req |> Request.meth |> Code.string_of_method in
  let path = req |> Request.uri |> Uri.path in
  Log.info "%s %s" meth path;
  ()

(* Create a server *)
let _ =
Log.set_log_level Log.DEBUG;
Log.set_output stdout;
let open Server in
server "127.0.0.1" 7888

(* JSON feed *)
>| get "/feed.json" (fun req params body ->
  log_req req; 
  Entry.iter (fun item -> 
    let uid = jform_field item ["properties"; "uid"] "" in
    let published = jform_field item ["properties"; "published"] "" in
    let content = jform_field item ["properties"; "content"] "" in
    let title = jform_field item ["properties"; "name"] "" in
    let slug = title |> slugify in
    let tags = jform_strings item ["properties"; "category"] in
    `O [
      "id", `String (build_url uid slug);
      "url", `String (build_url uid slug);
      "date_published", `String published;
      "content_html", `String (Omd.of_string content |> Omd.to_html);
      "title", `String title;
      "tags", Ezjsonm.(strings tags);

    ]
  ) >>= fun items ->
    let headers = Header.init ()
    |> set_content_type "application/json"
    |> add_header "Link" ("<" ^ base_url ^ "/feed.json>; rel=\"self\"")
    |> add_header "Link" ("<" ^ websub_endpoint ^ ">; rel=\"hub\"") in
    json (`O [
      (* TODO author avatar *)
      "author", `O ["name", `String author_name; "url", `String (base_url ^ "/")];
      "version", `String "https://jsonfeed.org/version/1";
      "title", `String blog_name;
      "home_page_url", `String (base_url ^ "/");
      "feed_url", `String (base_url ^ "/feed.json");
      "items", `A items;
      "hubs", `A [`String websub_endpoint];
    ]) ~headers)

(* JSON feed *)
>| head "/feed.json" (fun req params body ->
  log_req req; 
  let headers = Header.init ()
  |> set_content_type "application/json"
  |> add_header "Link" ("<" ^ base_url ^ "/feed.json>; rel=\"self\"")
  |> add_header "Link" ("<" ^ websub_endpoint ^ ">; rel=\"hub\"") in
  string "" ~headers)

(* Atom feed *)
>| get "/atom.xml" (fun req params body ->
  log_req req; 
  Entry.iter entry_tpl_data >>= fun entries ->
    (* Compute the "updated" field *)
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
    |> set_content_type "application/xml"
    |> add_header "Link" ("<" ^ base_url ^ "/atom.xml>; rel=\"self\"")
    |> add_header "Link" ("<" ^ websub_endpoint ^ ">; rel=\"hub\"") in
    string out ~headers)

>| head "/atom.xml" (fun req params body ->
  log_req req; 
  let headers = Header.init ()
  |> set_content_type "application/xml"
  |> add_header "Link" ("<" ^ base_url ^ "/atom.xml>; rel=\"self\"")
  |> add_header "Link" ("<" ^ websub_endpoint ^ ">; rel=\"hub\"") in
  string "" ~headers)

(* Index *)
>| get "/" (fun req params body ->
  log_req req; 
  Entry.iter entry_tpl_data >>= fun dat ->
    let dat = `O [
      "entries", `A dat;
      "base_url", `String base_url;
      "is_index", `Bool true;
      "is_entry", `Bool false;
      "is_404", `Bool false;
    ] in
    let out = Mustache.render html_tpl dat in
    let headers = Header.init ()
    |> set_content_type text_html
    |> add_micropub_header base_url in
    string out ~headers)

(* HEAD index *)
>| head "/" (fun req params body ->
  log_req req; 
  let headers = Header.init ()
  |> add_micropub_header base_url in
   string "" ~headers)

(* Micropub endpoint *)
>| post "/webmention" (fun req params body ->
  log_req req; 
  Webmention.process_incoming_webmention body)

(* Handle Micropub queries *)
>| get "/micropub" (fun req params body ->
  log_req req; 
  check_auth req >>= fun auth ->
  if auth = false then json_error "unauthorized" "bad token" 401 else
 micropub_query req)

(* Micropub endpoint *)
>| post "/micropub" (fun req params body ->
  log_req req; 
  check_auth req >>= fun auth ->
  if auth = false then json_error "unauthorized" "bad token" 401 else
  let content_type = Yurt_util.unwrap_option_default (Header.get req.Request.headers "Content-Type") "" in
  if content_type = "application/json" then
    handle_json_create body
  else
    handle_form_create body)

(* Post/entry page *)
>| get "/<uid:string>/<slug:string>" (fun req params body ->
  log_req req; 
  (* TODO check slug let slug = Route.string params "slug" in *)
  let uid = Route.string params "uid" in
  Entry.get uid >>= fun some_stored ->
    match some_stored with
    | Some stored ->
      (* Fetch associated Webmentions *)
      Webmention.iter uid (fun x -> x) >>= fun webmentions -> 
      (* Render the entry *)
      let nstored = stored |> entry_tpl_data in
      let has_webmentions = if List.(length webmentions) > 0 then true else false in
      let dat = `O [
        "is_index", `Bool false;
        "is_entry", `Bool true;
        "is_404", `Bool false;
        "base_url", `String base_url;
        "entry", nstored;
        "webmentions", `A webmentions;
        "has_webmentions", `Bool has_webmentions;
      ] in
      let out = Mustache.render html_tpl dat in
      let headers = Header.init ()
      |> set_content_type text_html
      |> add_micropub_header base_url
      |> add_webmention_header base_url in
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
     |> set_content_type text_html
     |> add_micropub_header base_url in
     string out ~headers ~status:404)
 
(* HEAD Post/entry page *)
>| head "/<uid:string>/<slug:string>" (fun req params body ->
  log_req req; 
  let uid = Route.string params "uid" in
  Entry.get uid >>= fun some_stored ->
    match some_stored with
    | Some _ ->
      let headers = Header.init ()
      |> set_content_type text_html
      |> add_micropub_header base_url
      |> add_webmention_header base_url in
      string "" ~headers
    | None ->
      (* 404 *)
      string "" ~status:404)


(* Run it *)
|> run
