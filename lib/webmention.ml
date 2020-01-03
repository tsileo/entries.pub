open Lwt.Infix
open Cohttp
open Cohttp.Link
include Soup
open Utils
open Config
open Opium.Std

exception Error_invalid_request of string

(* Build a webmention "object" from the given URL *)
let parse url soup =
  let title =
    try soup $ "title" |> fun node -> node |> R.leaf_text |> String.trim
    with _ -> url
  in
  let now = Date.now () in
  `O
    [ ("url", `String url)
    ; ("title", `String title)
    ; ("last_updated", `String (now |> Date.to_string)) ]

let link_uniq xs x = if List.mem x xs then xs else x :: xs

let dedup_links xs = List.fold_left link_uniq [] xs

(* Extract links from the HTML *)
let extract_links soup =
  soup $$ "a[href]" |> to_list |> List.map (R.attribute "href") |> dedup_links

(* Returns true if URL contains a link to target *)
let verify_incoming_webmention url target =
  try%lwt
        client_get url
        >|= fun (resp, code, body) ->
        (* Check the status code *)
        if code > 299 then (false, None)
        else
          (* Parse the HTML *)
          let soup = Soup.(parse body) in
          let links = soup |> extract_links in
          (* Check if it contains the target *)
          (List.mem target links, Some soup)
  with Failure _ -> (false, None) |> Lwt.return

let handle_relative_url url url_or_path =
  if url_or_path = "" then ""
  else if String.sub url_or_path 0 1 = "/" then
    Uri.(to_string (with_path (of_string url) url_or_path))
  else url_or_path

let discover_webmention_html url soup =
  try
    let u =
      soup $ "link[rel=webmention]" |> R.attribute "href"
      |> handle_relative_url url
    in
    if u = "" then None else Some u
  with Failure _ -> None

let rel_webmention = Link.Rel.extension (Uri.of_string "webmention")

let discover_webmention_header url resp =
  let links =
    Header.get_links (resp |> Cohttp.Response.headers)
    |> List.filter (fun {context; arc; target} ->
           List.exists (fun x -> x = rel_webmention) arc.relation )
  in
  if List.length links = 0 then None
  else
    List.hd links
    |> fun {target} -> Some (handle_relative_url url (Uri.to_string target))

(* Returns the Webmention endpoint for a given URL if any *)
let discover_webmention url =
  client_get url
  >|= fun (resp, code, body) ->
  (* Look for the webmention endpoint in the Link headers *)
  let via_header = discover_webmention_header url resp in
  match via_header with
  | Some _ -> via_header
  | None ->
      (* Look for the webmention endpoint in the <link> tag *)
      let soup = body |> Soup.parse in
      discover_webmention_html url soup

(* Send a webmention *)
let send_webmention source target webmention_url =
  (* Ensure a WebSub endpoint is set *)
  if Config.websub_endpoint = "" then Lwt.return true
  else
    (* Do the WebSub ping *)
    let params = [("source", [source]); ("target", [target])] in
    client_post_form ~params webmention_url
    >|= fun (resp, code, body) ->
    (* TODO log the webmention with result *)
    if code < 300 then true else false

(* Send webmentions to each target if a webmention endpoint is advertised *)
let send_webmentions source body =
  let targets =
    extract_links Soup.(parse body)
    (* filter "local" links *)
    |> List.filter (fun url ->
           Uri.(host (of_string url)) <> Uri.(host (of_string base_url)) )
  in
  Lwt_list.map_s
    (fun target ->
      discover_webmention target
      >>= fun wu ->
      match wu with
      | Some webmention_url -> send_webmention source target webmention_url
      | None -> Lwt.return false )
    targets

let add_last_updated_pretty dat =
  let sdate = Ezjsonm.(get_string (find dat ["last_updated"])) in
  let ndat = Ezjsonm.get_dict dat in
  `O
    ( ndat
    @ [ ( "last_updated_pretty"
        , `String (Date.of_string sdate |> Date.to_pretty) ) ] )

(* List webmention for a given uid *)
let iter uid map =
  Store.Repo.v config >>= Store.master
  >>= fun t ->
  Store.list t ["webmentions"; uid]
  >>= fun keys ->
  Lwt_list.map_s
    (fun (s, c) ->
      Store.get t ["webmentions"; uid; s]
      >|= fun stored ->
      stored |> Ezjsonm.from_string |> add_last_updated_pretty )
    keys

(* Save/update a webmention *)
let save_webmention uid mention js =
  Store.Repo.v config >>= Store.master
  >>= fun t ->
  (* TODO fix the JSON dump/serialize here
   * let js = Ezjsonm.(to_string (value (`O mention))) in *)
  let u = Ezjsonm.(get_string (find mention ["url"])) in
  let wid = new_id () in
  Store.list t ["webmentions"; uid]
  >>= fun keys ->
  Lwt_list.map_s
    (fun (s, c) ->
      Store.get t ["webmentions"; uid; s]
      >|= fun stored ->
      if Ezjsonm.(get_string (find (from_string stored) ["url"])) = u then
        (true, s)
      else (false, s) )
    keys
  >>= fun dat ->
  Lwt_list.filter_s
    (fun (ok, id) -> if ok then Lwt.return true else Lwt.return false)
    dat
  >>= fun res ->
  if List.length res > 0 then
    let _, nid = List.hd res in
    Store.set t
      ~info:(info "Updating webmention %s for entry %s" nid uid)
      ["webmentions"; uid; nid] js
  else
    Store.set t
      ~info:(info "Creating webmention %s for entry %s" wid uid)
      ["webmentions"; uid; wid] js

(* Sanity check before fetching a source webmention *)
let bad_url u =
  let uri = Uri.(of_string u) in
  let scheme = unwrap_option Uri.(scheme uri) "" in
  (* Only accept http/https URL *)
  if scheme = "" || not (scheme = "http" || scheme = "https") then true
  else
    let host = unwrap_option Uri.(host uri) "" in
    try
      let _ = Ipaddr.(of_string_exn host) in
      (* Reject raw IP addr *)
      true
    with Ipaddr.Parse_error (_, _) ->
      (* Also reject localhost and invalid URLs *)
      if host = "localhost" || host = "" then true else false

(* Process incoming Webmentions *)
let process_incoming_webmention dat =
  let source = form_value dat "source" "" in
  let target = form_value dat "target" "" in
  (* Sanity checks *)
  if source = "" then raise (Error_invalid_request "missing source")
  else if target = "" then raise (Error_invalid_request "missing target")
  else if bad_url source then raise (Error_invalid_request "invalid source")
  else if Uri.(host (of_string target)) <> Uri.(host (of_string base_url)) then
    raise (Error_invalid_request "invalid target")
  else
    let uid, _ = get_uid_and_slug (target |> Uri.of_string |> Uri.path) in
    (* Verify the Webmention (by looking for the target in the source *)
    verify_incoming_webmention source target
    >>= fun (ok, soup) ->
    match soup with
    | Some soup ->
        if ok then (
          let dat = parse source soup in
          save_webmention uid dat Ezjsonm.(to_string dat)
          >>= fun _ -> `String "" |> respond' )
        else raise (Error_invalid_request "target not found in source")
    | None -> raise (Error_invalid_request "unreachable source")
