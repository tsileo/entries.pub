open Lwt.Infix
open Opium.Std

open Entriespub.Utils
open Entriespub.Config

let logger =
  let filter handler req =
    let start = Unix.gettimeofday () in
    let uri = Request.uri req |> Uri.path_and_query in
    let headers = Request.headers req in
    let ua = get_header headers "User-Agent" in
    let referer = get_header headers "Referer" in
    let date = Date.now () |> Date.to_string in
    handler req
    >|= fun response ->
    let rheaders = response |> Response.headers in
    let code = response |> Response.code |> Cohttp.Code.code_of_status in
    let meth = req |> Request.meth |> Cohttp.Code.string_of_method in
    Logs.info (fun m -> m "[%s] - \"%s %s HTTP/1.1\" %d \"%s\" \"%s\" %f" date meth uri code referer ua (Unix.gettimeofday () -. start)) ;
    response
  in
  Rock.Middleware.create ~name:"Logger" ~filter

(* Call the token endpoint in order to verify the token validity *)
let check_auth req =
  (* Get the bearer token from the incoming request *)
  let headers = Cohttp.Request.headers req in
  let auth = get_header headers "Authorization" in
  (* Skip the validation if the request has no `Authorization` header *)
  if auth = "" then Lwt.return (false, []) else
  (* And forward it to the token endpoint *)
  let headers = Cohttp.Header.init ()
  |> add_header "Authorization" auth
  |> add_header "Accept" "application/json" in

  client_get ~headers token_endpoint >|= fun (resp, code, body) ->
    Logs.info (fun m -> m "Called token endopint with %d %s" code body);
    match resp with
    | { Cohttp.Response.status = `OK } -> 
        (* The token is validated, now we need to parse the response to extract the scopes *)
		let js = Ezjsonm.(from_string body) in
		(* The scope field is a space-separated list of scopes, e.g. `create update delete` *)
		let scopes =  Str.split (Str.regexp_string " ") (jdata_field js ["scope"] "") in
        (true, scopes)
    | _ ->
      (* Bad token *)
      (false, [])

(* GET JSON Feed *)
let get_feed_json =
  App.get "/feed.json" (fun req ->
  Entriespub.Entry.iter (fun item ->
    let tags = jform_strings item ["properties"; "category"] in
    let is_page = if List.mem "page" tags then true else false in
    let uid = jform_field item ["properties"; "uid"] "" in
    let published = jform_field item ["properties"; "published"] "" in
    let content = jform_field item ["properties"; "content"] "" in
    let title = jform_field item ["properties"; "name"] "" in
    let slug = title |> Entriespub.Entry.slugify in
    if is_page then `O ["is_page", `Bool true;] else
    `O [
      "id", `String (build_url uid slug);
      "url", `String (build_url uid slug);
      "date_published", `String published;
      "content_html", `String (content |> Omd.of_string |> Omd.to_html);
      "title", `String title;
      "tags", Ezjsonm.(strings tags);

    ]
  ) >>= Entriespub.Entry.discard_pages >>= fun items -> 
  let headers = Cohttp.Header.init ()
    |> add_header "Link" ("<" ^ base_url ^ "/feed.json>; rel=\"self\"")
    |> add_header "Link" ("<" ^ websub_endpoint ^ ">; rel=\"hub\"") in
  `Json (`O [
      (* TODO author avatar *)
      "author", `O ["name", `String author_name; "url", `String (base_url ^ "/")];
      "version", `String "https://jsonfeed.org/version/1";
      "title", `String blog_name;
      "home_page_url", `String (base_url ^ "/");
      "feed_url", `String (base_url ^ "/feed.json");
      "items", `A items;
      "hubs", `A [`String websub_endpoint];
    ]) |> respond' ~headers)

(* HEAD JSON Feed *)
let head_feed_json =
  App.head "/feed.json" (fun req ->
  let headers = Cohttp.Header.init ()
  |> set_content_type "application/json"
  |> add_header "Link" ("<" ^ base_url ^ "/feed.json>; rel=\"self\"")
  |> add_header "Link" ("<" ^ websub_endpoint ^ ">; rel=\"hub\"") in
  `String "" |> respond' ~headers)

(* GET Atom feed *)
let get_feed_atom =
  App.get "/atom.xml" (fun req ->
  Entriespub.Entry.iter Entriespub.Entry.entry_tpl_data >>=
  Entriespub.Entry.discard_pages >>= fun entries ->
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
    let headers = Cohttp.Header.init ()
    |> set_content_type "application/xml"
    |> add_header "Link" ("<" ^ base_url ^ "/atom.xml>; rel=\"self\"")
    |> add_header "Link" ("<" ^ websub_endpoint ^ ">; rel=\"hub\"") in
    `Xml out |> respond' ~headers)

(* HEAD Atom feed *)
let head_feed_atom =
  App.get "/atom.xml" (fun req ->
  let headers = Cohttp.Header.init ()
  |> set_content_type "application/xml"
  |> add_header "Link" ("<" ^ base_url ^ "/atom.xml>; rel=\"self\"")
  |> add_header "Link" ("<" ^ websub_endpoint ^ ">; rel=\"hub\"") in
  `String "" |> respond' ~headers)

(* GET Index *)
let get_index =
  App.get "/" (fun req ->
  Entriespub.Entry.iter Entriespub.Entry.entry_tpl_data >>= fun dat ->
    let dat = `O [
      "entries", `A dat;
      "pages", `A [];
      "base_url", `String base_url;
      "is_index", `Bool true;
      "is_entry", `Bool false;
      "is_404", `Bool false;
    ] in
    let out = Mustache.render html_tpl dat in
    let headers = Cohttp.Header.init ()
    |> add_micropub_header base_url in
    `Html out |> respond' ~headers)

(* HEAD Index *)
let head_index =
  App.get "/" (fun req ->
  let headers = Cohttp.Header.init ()
  |> add_micropub_header base_url in
   `Html "" |> respond' ~headers)

(* GET a singe entry *)
let get_entry =
  App.get "/:uid/:slug" (fun req ->
  (* TODO check slug let slug = Route.string params "slug" in *)
  let uid = param req "uid" in
  Entriespub.Entry.get uid >>= fun some_stored ->
    match some_stored with
    | Some stored ->
      (* Fetch associated Webmentions *)
      Entriespub.Webmention.iter uid (fun x -> x) >>= fun webmentions ->
      (* Render the entry *)
      let nstored = stored |> Entriespub.Entry.entry_tpl_data in
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
      let headers = Cohttp.Header.init ()
      |> add_micropub_header base_url
      |> add_webmention_header base_url in
      (`Html out |> respond' ~headers)
   | None ->
     (* Return a 404 *)
     let dat = `O [
       "is_index", `Bool false;
       "is_entry", `Bool false;
       "is_404", `Bool true;
       "base_url", `String base_url;
     ] in
     let out = Mustache.render html_tpl dat in
     let headers = Cohttp.Header.init ()
     |> add_micropub_header base_url in
     `Html out |> respond' ~headers ~code:`Not_found)

(* HEAD a single entry *)
let head_entry =
  App.head "/:uid/:slug" (fun req ->
  let uid = param req "uid" in
  Entriespub.Entry.get uid >>= fun some_stored ->
    match some_stored with
    | Some _ ->
      let headers = Cohttp.Header.init ()
      |> add_micropub_header base_url
      |> add_webmention_header base_url in
      `Html "" |> respond' ~headers
    | None ->
      (* 404 *)
      `Html "" |> respond' ~code:`Not_found)

(* GET handle Micropub queries *)
let get_micropub =
  App.get "/micropub" (fun req ->
  check_auth req.request >>= fun (auth, scopes) ->
  (* No scopes checking, we're sure to have at least one (otherwise no token would have been generated) *)
  if auth = false then
    `Json (build_error "unauthorized" "bad token") |> respond' ~code:`Unauthorized
  else
  try%lwt Entriespub.Micropub.micropub_query req
  with Entriespub.Micropub.Error_invalid_request m ->
    `Json (build_error "invalid_request" m) |> respond' ~code:`Bad_request)

(* POST Micropub endpoint *)
let post_micropub =
  App.post "/micropub" (fun req ->
  check_auth req.request >>= fun (auth, scopes) ->
  if auth = false then
    `Json (build_error "unauthorized" "bad token") |> respond' ~code:`Unauthorized
  else
  let headers = Cohttp.Request.headers req.request in
  let content_type = get_header headers "Content-Type" in
  (* Choose between the JSON/FORM Micropub handler *)
  try%lwt if content_type = "application/json" then 
    req |> App.json_of_body_exn >>= fun dat ->
      Entriespub.Micropub.handle_json_create Ezjsonm.(value dat) scopes
  else
    req |> App.urlencoded_pairs_of_body >>= fun dat ->
      Entriespub.Micropub.handle_form_create dat scopes
  with 
  | Entriespub.Micropub.Error_insufficient_scope m ->
    `Json (build_error "insufficient_scope" m) |> respond' ~code:`Unauthorized
  | Entriespub.Micropub.Error_invalid_request m ->
    `Json (build_error "invalid_request" m) |> respond' ~code:`Bad_request
  | Entriespub.Micropub.Error_not_found m ->
    `Json (build_error "not_found" m) |> respond' ~code:`Not_found)

(* POST Micropub endpoint *)
let post_webmention =
  App.post "/webmention" (fun req ->
  req |> App.urlencoded_pairs_of_body >>= fun dat ->
  try%lwt Entriespub.Webmention.process_incoming_webmention dat
  with Entriespub.Webmention.Error_invalid_request m ->
    `Json (build_error "invalid_request" m) |> respond' ~code:`Bad_request)

let () =
  let static = Middleware.static ~local_path:"./static" ~uri_prefix:"/static" in 
  let app = App.empty
  |> head_feed_json
  |> get_feed_json
  |> head_feed_atom
  |> get_feed_atom
  |> head_index
  |> get_index
  |> head_entry
  |> get_entry
  |> post_webmention
  |> get_micropub
  |> post_micropub
  |> middleware static
  |> middleware logger
  |> App.port 7888
  |> App.run_command' in
  match app with
  | `Ok app ->
      let s =
        begin
          Logs.set_reporter (Logs_fmt.reporter ()) ;
          Logs.set_level (Some Logs.Info) ;
          app
        end
      in
      ignore (Lwt_main.run s)
  | `Error -> exit 1
  | `Not_running -> exit 0
