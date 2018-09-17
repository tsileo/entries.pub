open Lwt.Infix
open Yurt
include Cohttp
include Cohttp.Link
open Cohttp
include Soup

let get_links soup =
  soup $$ "a[href]"
  |> to_list |> Lwt.return
 
let verify_incoming_webmention url target =
  Client.get url >>= fun (resp, body) ->
  let soup = body |> Soup.parse in
  get_links soup >>= fun olinks ->
    let links = olinks |> List.map (fun x -> x |> R.attribute "href") in
    List.mem target links |> Lwt.return

let discover_webmention_html soup =
  get_links soup >>= fun olinks ->
    let links = olinks
    |> List.filter (fun x ->
      let rel = x |> R.attribute "rel" in
      if rel = "webmention" then true else false)
    |> List.map (fun x -> x |> R.attribute "href") in
  if List.length links > 0 then
    let first = List.hd links in
    Some first |> Lwt.return
  else
    None |> Lwt.return

let rel_webmention = Link.Rel.extension (Uri.of_string "webmention")

let discover_webmention_header resp =
  let links = 
    Header.get_links (resp |> Response.headers)
    |> List.filter (fun ({ context; arc; target }) -> List.hd arc.relation = rel_webmention) in
  if List.length links = 0 then None else
  List.hd links |> (fun ({target}) -> Some (Uri.to_string target))

let discover_webmention url =
  Client.get url >>= fun (resp, body) ->
  let via_header = discover_webmention_header resp in
  match via_header with
  | Some u -> Some u |> Lwt.return
  | None ->
    let soup = body |> Soup.parse in
    discover_webmention_html soup
