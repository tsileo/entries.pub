open Lwt.Infix
open Yurt
include Cohttp
include Cohttp.Link
open Cohttp
include Soup

(* Returns true if URL contains a link to target *)
let verify_incoming_webmention url target =
  Client.get url >>= fun (resp, body) ->
    let soup = body |> Soup.parse in
    let links =
      soup $$ "a[href]"
      |> to_list
      |> List.map (fun x -> x |> R.attribute "href")
    in
    List.mem target links |> Lwt.return

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
