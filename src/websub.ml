open Lwt.Infix
open Yurt
open Cohttp
open Soup

let ping updated =
  let params = ["hub.mode",["publish"]; "hub.url",[updated]] in
  Client.post_form ~params "https://pubsubhubbub.superfeedr.com/" >>= fun (resp, body) ->
  let code =
    resp 
    |> Response.status 
    |> Code.code_of_status
  in
  if code = 204 then true else false
  |> Lwt.return
