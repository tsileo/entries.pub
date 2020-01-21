open Lwt.Infix
open Opium.Std
open Entriespub
open Entriespub.Utils
open Entriespub.Config

let logger =
  let filter handler req =
    let start = Unix.gettimeofday () in
    let uri = Request.uri req |> Uri.path_and_query in
    let headers = Request.headers req in
    let ua = get_header headers "User-Agent" in
    let referer = get_header headers "Referer" in
    let date = Datetime.now () |> Datetime.to_string in
    handler req
    >|= fun response ->
    let rheaders = response |> Response.headers in
    let code = response |> Response.code |> Cohttp.Code.code_of_status in
    let meth = req |> Request.meth |> Cohttp.Code.string_of_method in
    Logs.info (fun m ->
        m
          "[%s] - \"%s %s HTTP/1.1\" %d \"%s\" \"%s\" %f"
          date
          meth
          uri
          code
          referer
          ua
          (Unix.gettimeofday () -. start)) ;
    response
  in
  Rock.Middleware.create ~name:"Logger" ~filter


(* Call the token endpoint in order to verify the token validity *)
let check_auth req =
  (* Get the bearer token from the incoming request *)
  let headers = Cohttp.Request.headers req in
  let auth = get_header headers "Authorization" in
  (* Skip the validation if the request has no `Authorization` header *)
  if auth = ""
  then Lwt.return (false, [])
  else
    (* And forward it to the token endpoint *)
    let headers =
      Cohttp.Header.init ()
      |> add_header "Authorization" auth
      |> add_header "Accept" "application/json"
    in
    client_get ~headers token_endpoint
    >|= fun (resp, code, body) ->
    Logs.info (fun m -> m "Called token endopint with %d %s" code body) ;
    match resp with
    | { Cohttp.Response.status = `OK } ->
        (* The token is validated, now we need to parse the response to extract the scopes *)
        let js = Ezjsonm.(from_string body) in
        (* The scope field is a space-separated list of scopes, e.g. `create update delete` *)
        let scopes =
          Str.split (Str.regexp_string " ") (jdata_field js [ "scope" ] "")
        in
        (true, scopes)
    | _ ->
        (* Bad token *)
        (false, [])


(* GET JSON Feed *)
let get_feed_json =
  App.get "/feed.json" (fun req ->
      Entry.iter (fun item ->
          let tags = jform_strings item [ "properties"; "category" ] in
          let is_page = if List.mem "page" tags then true else false in
          let uid = jform_field item [ "properties"; "uid" ] "" in
          let published = jform_field item [ "properties"; "published" ] "" in
          let content = jform_field item [ "properties"; "content" ] "" in
          let title = jform_field item [ "properties"; "name" ] "" in
          let slug = title |> Entry.slugify in
          if is_page
          then `O [ ("is_page", `Bool true) ]
          else
            `O
              [ ("id", `String (build_url uid slug))
              ; ("url", `String (build_url uid slug))
              ; ("date_published", `String published)
              ; ( "content_html"
                , `String (content |> Omd.of_string |> Omd.to_html) )
              ; ("title", `String title)
              ; ("tags", Ezjsonm.(strings tags))
              ])
      >>= Entry.discard_pages_and_drafts
      >>= fun items ->
      let headers =
        Cohttp.Header.init ()
        |> add_header "Link" ("<" ^ base_url ^ "/feed.json>; rel=\"self\"")
        |> add_header "Link" ("<" ^ websub_endpoint ^ ">; rel=\"hub\"")
      in
      `Json
        (`O
          [ (* TODO author avatar *)
            ( "author"
            , `O
                [ ("name", `String author_name)
                ; ("url", `String (base_url ^ "/"))
                ] )
          ; ("version", `String "https://jsonfeed.org/version/1")
          ; ("title", `String blog_name)
          ; ("home_page_url", `String (base_url ^ "/"))
          ; ("feed_url", `String (base_url ^ "/feed.json"))
          ; ("items", `A items)
          ; ("hubs", `A [ `String websub_endpoint ])
          ])
      |> respond' ~headers)


(* HEAD JSON Feed *)
let head_feed_json =
  App.head "/feed.json" (fun req ->
      let headers =
        Cohttp.Header.init ()
        |> set_content_type "application/json"
        |> add_header "Link" ("<" ^ base_url ^ "/feed.json>; rel=\"self\"")
        |> add_header "Link" ("<" ^ websub_endpoint ^ ">; rel=\"hub\"")
      in
      `String "" |> respond' ~headers)


(* GET Atom feed *)
let get_feed_atom =
  App.get "/atom.xml" (fun req ->
      Entry.iter (fun x ->
          x |> Entry.EntryModel.of_json |> Entry.EntryModel.to_tpl)
      >>= Entry.discard_pages_and_drafts
      >>= fun entries ->
      (* Compute the "updated" field *)
      let updated =
        if List.length entries > 0
        then
          let last_one = List.hd entries in
          Ezjsonm.(get_string (find last_one [ "published" ]))
        else ""
      in
      let dat =
        `O
          [ ("websub_endpoint", `String websub_endpoint)
          ; ("name", `String blog_name)
          ; ("base_url", `String base_url)
          ; ("updated", `String updated)
          ; ("entries", `A entries)
          ]
      in
      let out = Mustache.render atom_tpl dat in
      let headers =
        Cohttp.Header.init ()
        |> set_content_type "application/xml"
        |> add_header "Link" ("<" ^ base_url ^ "/atom.xml>; rel=\"self\"")
        |> add_header "Link" ("<" ^ websub_endpoint ^ ">; rel=\"hub\"")
      in
      `Xml out |> respond' ~headers)


(* HEAD Atom feed *)
let head_feed_atom =
  App.get "/atom.xml" (fun req ->
      let headers =
        Cohttp.Header.init ()
        |> set_content_type "application/xml"
        |> add_header "Link" ("<" ^ base_url ^ "/atom.xml>; rel=\"self\"")
        |> add_header "Link" ("<" ^ websub_endpoint ^ ">; rel=\"hub\"")
      in
      `String "" |> respond' ~headers)


let render_entry some_stored =
  match some_stored with
  | Some stored ->
      (* Fetch associated Webmentions *)
      let uid = jform_field stored [ "properties"; "uid" ] "" in
      Webmention.iter uid (fun x -> x)
      >>= fun webmentions ->
      (* Render the entry *)
      let nstored =
        stored |> Entry.EntryModel.of_json |> Entry.EntryModel.to_tpl
      in
      let has_webmentions =
        if List.(length webmentions) > 0 then true else false
      in
      let dat =
        `O
          [ ("is_index", `Bool false)
          ; ("is_entry", `Bool true)
          ; ("is_host", `Bool Ezjsonm.(get_bool (find nstored [ "is_host" ])))
          ; ("is_404", `Bool false)
          ; ("base_url", `String base_url)
          ; ("entry", nstored)
          ; ("webmentions", `A webmentions)
          ; ("has_webmentions", `Bool has_webmentions)
          ; ("token_endpoint", `String token_endpoint)
          ; ("authorization_endpoint", `String authorization_endpoint)
          ; ("author_name", `String author_name)
          ; ("author_icon", `String author_icon)
          ; ("hero", `String hero)
          ; ("blog_name", `String blog_name)
          ]
      in
      let out = Mustache.render html_tpl dat in
      let headers =
        Cohttp.Header.init ()
        |> add_micropub_header base_url
        |> add_webmention_header base_url
      in
      `Html out |> respond' ~headers
  | None ->
      (* Return a 404 *)
      let dat =
        `O
          [ ("is_index", `Bool false)
          ; ("is_entry", `Bool false)
          ; ("is_404", `Bool true)
          ; ("base_url", `String base_url)
          ; ("token_endpoint", `String token_endpoint)
          ; ("authorization_endpoint", `String authorization_endpoint)
          ; ("author_name", `String author_name)
          ; ("author_icon", `String author_icon)
          ; ("hero", `String hero)
          ; ("blog_name", `String blog_name)
          ]
      in
      let out = Mustache.render html_tpl dat in
      let headers = Cohttp.Header.init () |> add_micropub_header base_url in
      `Html out |> respond' ~headers ~code:`Not_found


(* GET Index *)
let get_index =
  App.get "/" (fun req ->
      (* First, check if the requested host match a special mp-host entry. *)
      let headers = Cohttp.Request.headers req.request in
      let rhost = get_header headers "Host" in
      Entry.get rhost
      >>= fun some_stored ->
      match some_stored with
      | Some stored ->
          render_entry (Some stored)
      | None ->
          Entry.iter (fun x ->
              x |> Entry.EntryModel.of_json |> Entry.EntryModel.to_tpl)
          >>= fun raw ->
          let dat =
            `O
              [ ("entries", `A raw)
              ; ("pages", `A [])
              ; ("base_url", `String base_url)
              ; ("is_index", `Bool true)
              ; ("is_entry", `Bool false)
              ; ("is_host", `Bool false)
              ; ("is_404", `Bool false)
              ; ("token_endpoint", `String token_endpoint)
              ; ("authorization_endpoint", `String authorization_endpoint)
              ; ("blog_name", `String blog_name)
              ; ("author_name", `String author_name)
              ; ("author_icon", `String author_icon)
              ; ("hero", `String hero)
              ]
          in
          let out = Mustache.render html_tpl dat in
          let headers = Cohttp.Header.init () |> add_micropub_header base_url in
          `Html out |> respond' ~headers)


(* HEAD Index *)
let head_index =
  App.head "/" (fun req ->
      let headers = Cohttp.Header.init () |> add_micropub_header base_url in
      `Html "" |> respond' ~headers)


(* GET a singe entry *)
let get_entry =
  App.get "/:uid/:slug" (fun req ->
      (* TODO check slug let slug = Route.string params "slug" in *)
      let uid = param req "uid" in
      Entry.get uid >>= fun some_stored -> render_entry some_stored)


(* HEAD a single entry *)
let head_entry =
  App.head "/:uid/:slug" (fun req ->
      let uid = param req "uid" in
      Entry.get uid
      >>= fun some_stored ->
      match some_stored with
      | Some _ ->
          let headers =
            Cohttp.Header.init ()
            |> add_micropub_header base_url
            |> add_webmention_header base_url
          in
          `Html "" |> respond' ~headers
      | None ->
          (* 404 *)
          `Html "" |> respond' ~code:`Not_found)


(* GET handle Micropub queries *)
let get_micropub =
  App.get "/micropub" (fun req ->
      check_auth req.request
      >>= fun (auth, scopes) ->
      (* No scopes checking, we're sure to have at least one (otherwise no token would have been generated) *)
      if auth = false
      then
        `Json (build_error "unauthorized" "bad token")
        |> respond' ~code:`Unauthorized
      else
        try%lwt Micropub.micropub_query req with
        | Micropub.Error_invalid_request m ->
            `Json (build_error "invalid_request" m)
            |> respond' ~code:`Bad_request)


(* POST Micropub endpoint *)
let post_micropub =
  App.post "/micropub" (fun req ->
      check_auth req.request
      >>= fun (auth, scopes) ->
      if auth = false
      then
        `Json (build_error "unauthorized" "bad token")
        |> respond' ~code:`Unauthorized
      else
        let headers = Cohttp.Request.headers req.request in
        let content_type = get_header headers "Content-Type" in
        (* Choose between the JSON/FORM Micropub handler *)
        try%lwt
          if content_type = "application/json"
          then
            req
            |> App.json_of_body_exn
            >>= fun dat ->
            Micropub.handle_json_create Ezjsonm.(value dat) scopes
          else
            req
            |> App.urlencoded_pairs_of_body
            >>= fun dat -> Micropub.handle_form_create dat scopes
        with
        | Micropub.Error_insufficient_scope m ->
            `Json (build_error "insufficient_scope" m)
            |> respond' ~code:`Unauthorized
        | Micropub.Error_invalid_request m ->
            `Json (build_error "invalid_request" m)
            |> respond' ~code:`Bad_request
        | Micropub.Error_not_found m ->
            `Json (build_error "not_found" m) |> respond' ~code:`Not_found)


(* Process incoming Webmentions *)
let process_incoming_webmention dat =
  let source = form_value dat "source" "" in
  let target = form_value dat "target" "" in
  (* Sanity checks *)
  if source = ""
  then raise (Webmention.Error_invalid_request "missing source")
  else if target = ""
  then raise (Webmention.Error_invalid_request "missing target")
  else if Webmention.bad_url source
  then raise (Webmention.Error_invalid_request "invalid source")
  else
    let uri = target |> Uri.of_string in
    let rhost = source_of_uri uri in
    let uid, slug =
      try get_uid_and_slug (uri |> Uri.path) with _ -> ("", "")
    in
    Entry.get_by_uid_or_host uid rhost
    >>= fun some_stored ->
    match some_stored with
    | Some stored ->
        let entry = stored |> Entry.EntryModel.of_json in
        let uid = Entry.get_prop_exn entry.properties.uid in
        let url = Entry.get_prop_exn entry.properties.url in
        (* Verify the Webmention (by looking for the target in the source *)
        Webmention.verify_incoming_webmention source url
        >>= fun (ok, soup) ->
        ( match soup with
        | Some soup ->
            if ok
            then
              let dat = Webmention.parse source soup in
              Webmention.save_webmention uid dat Ezjsonm.(to_string dat)
              >>= fun _ -> `String "" |> respond'
            else
              raise
                (Webmention.Error_invalid_request "target not found in source")
        | None ->
            raise (Webmention.Error_invalid_request "unreachable source")
        | None ->
            raise (Webmention.Error_invalid_request "invalid target") )


(* POST Micropub endpoint *)
let post_webmention =
  App.post "/webmention" (fun req ->
      req
      |> App.urlencoded_pairs_of_body
      >>= fun dat ->
      try%lwt process_incoming_webmention dat with
      | Webmention.Error_invalid_request m ->
          `Json (build_error "invalid_request" m) |> respond' ~code:`Bad_request)


let () =
  let static =
    Middleware.static ~local_path:"./static" ~uri_prefix:"/static" ()
  in
  let app =
    App.empty
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
    |> App.run_command'
  in
  match app with
  | `Ok app ->
      let s =
        Logs.set_reporter (Logs_fmt.reporter ()) ;
        Logs.set_level (Some Logs.Info) ;
        app
      in
      ignore (Lwt_main.run s)
  | `Error ->
      exit 1
  | `Not_running ->
      exit 0
