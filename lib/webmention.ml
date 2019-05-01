open Lwt.Infix
open Yurt
include Cohttp
include Cohttp.Link
open Cohttp
include Soup

open Utils
open Config

(* Build a webmention "object" from the given URL *)
let parse url soup =
  let title = try
    soup $ "title" |> (fun node ->
      node |> R.leaf_text |> String.trim
    )
  with _ -> url in
  `O [
    "url", `String url;
    "title", `String title;
    "last_updated", `String (Date.now () |> Date.to_string);
  ]

(* Returns true if URL contains a link to target *)
let verify_incoming_webmention url target =
  try%lwt Client.get url >>= fun (resp, body) ->
    (* TODO check for 404/410 with custom exn *)
    let soup = body |> Soup.parse in
    let links =
      soup $$ "a[href]"
      |> to_list
      |> List.map (R.attribute "href")
    in
    (List.mem target links, Some soup) |> Lwt.return
  with Failure _ ->
    (false, None) |> Lwt.return

let discover_webmention_html soup =
  try
    soup $ "link[rel=webmention]" |> R.attribute "href" |> fun u -> Some u |> Lwt.return
  with Failure _ ->
    None |> Lwt.return

let rel_webmention = Link.Rel.extension (Uri.of_string "webmention")

let discover_webmention_header resp =
  let links = 
    Header.get_links (resp |> Response.headers)
    |> List.filter (fun { context; arc; target } -> List.exists (fun x -> x = rel_webmention) arc.relation)
  in
  if List.length links = 0 then None else
  List.hd links |> (fun {target} -> Some (Uri.to_string target))

(* Returns the Webmention endpoint for a given URL if any *)
let discover_webmention url =
  Client.get url >>= fun (resp, body) ->
    (* Look for the webmention endpoint in the Link headers *)
    let via_header = discover_webmention_header resp in
    match via_header with
    | Some u -> Some u |> Lwt.return
    | None ->
      (* Look for the webmention endpoint in the <link> tag *)
      let soup = body |> Soup.parse in
      discover_webmention_html soup

(* TODO List webmention for a given uid *)
let iter uid =
  ()

(* Save/update a webmention *)
let save_webmention uid mention js =
  Store.Repo.v config >>=
  Store.master >>= fun t ->
  (* TODO fix the JSON dump/serialize here
   * let js = Ezjsonm.(to_string (value (`O mention))) in *)
  let u = Ezjsonm.(get_string (find mention ["url"])) in
  let wid = new_id () in
  Store.list t ["webmentions"; uid] >>= fun keys ->
    (Lwt_list.map_s (fun (s, c) ->
    Store.get t ["webmentions"; uid; s] >>= fun stored ->
        if Ezjsonm.(get_string (find (from_string stored) ["url"])) = u then
          Lwt.return (true, s) 
        else
          Lwt.return (false, s)
    ) keys) >>= fun dat ->
    (Lwt_list.filter_s (fun (ok, id) ->
        if ok then
          Lwt.return true
        else
          Lwt.return false
    ) dat) >>= fun res ->
      if List.length res > 0 then
       let (_, nid) = List.hd res in
        Store.set t ~info:(info "Updating webmention %s for entry %s" nid uid) ["webmentions"; uid; nid] js
      else
        Store.set t ~info:(info "Creating webmention %s for entry %s" wid uid) ["webmentions"; uid; wid] js

(* Sanity check before fetching a source webmention *)
let bad_url u =
  let uri = Uri.(of_string u) in
  let scheme = Yurt_util.unwrap_option_default Uri.(scheme uri) "" in
  (* Only accept http/https URL *)
  if scheme = "" || not (scheme = "http" || scheme = "https") then
    true
  else
  let host = Yurt_util.unwrap_option_default Uri.(host uri) "" in
  try let i = Ipaddr.(of_string_exn host) in
    (* Reject raw IP addr *)
    true
  with Ipaddr.Parse_error (_, _) ->
    (* Also reject localhost and invalid URLs *)
    if host = "localhost" || host = "" then
      true
    else
      false

(* Process incoming Webmentions *)
let process_incoming_webmention body =
  Form.urlencoded_json body >>= fun p ->
    Log.info "[Webmention] processing payload\n%s" Ezjsonm.(to_string p);
    let jdata = Ezjsonm.(value p) in
    let source = jform_field jdata ["source"] "" in
    let target = jform_field jdata ["target"] "" in
    (* Sanity checks *)
    if source = "" then
      Server.json (invalid_request_error "missing source") ~status:400
    else if target = "" then
      Server.json (invalid_request_error "missing target") ~status:400
    else if bad_url source then
      Server.json (invalid_request_error "invalid source") ~status:400
    else if Uri.(host (of_string target)) <> Uri.(host (of_string base_url)) then
      Server.json (invalid_request_error "invalid target") ~status:400
    else
      let (uid, _) = get_uid_and_slug (target |> Uri.of_string |> Uri.path) in 
      (* Verify the Webmention (by looking for the target in the source *)
      verify_incoming_webmention source target >>= fun (ok, soup) ->
      match soup with
      | Some soup ->
        if ok then
          let dat = parse source soup in
          Log.info "[Webmention] parsed mf\n%s" Ezjsonm.(to_string dat);
          save_webmention uid dat Ezjsonm.(to_string dat) >>= fun _ ->
            Server.string ""
        else
          Server.json (invalid_request_error "target not found in source") ~status:400
      | None ->
        Server.json (invalid_request_error "unreachable source") ~status:400

(* TODO send webmention *)
let send_webmention () = ()
