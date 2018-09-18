open Lwt.Infix
open Yurt
open Cohttp
open Soup

open Config

(* Notify the WebSub hub that a resource has been updated *)
let ping updated =
  (* Ensure a WebSub endpoint is set *)
  if websub_endpoint = "" then Lwt.return true else
  (* Do the WebSub ping *)
  let params = ["hub.mode",["publish"]; "hub.url",[updated]] in
  Client.post_form ~params websub_endpoint >>= fun (resp, body) ->
    let code =
      resp 
      |> Response.status 
      |> Code.code_of_status
    in
    if code = 204 then true else false
    |> Lwt.return
