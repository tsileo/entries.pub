open Lwt.Infix
open Yurt
open Cohttp
open Soup

let verify_webmention url target =
  Client.get url >>= fun (resp, body) ->
  let soup = body |> parse in
  let links = soup $$ "a[href]"
    |> to_list
    |> List.map (fun x -> x |> R.attribute "href") in
  List.mem target links |> Lwt.return

