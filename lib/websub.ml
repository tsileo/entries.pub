open Lwt.Infix
open Yurt
open Cohttp
open Soup

open Entriespub

(* Notify the WebSub hub that a resource has been updated *)
let ping updated =
  (* Ensure a WebSub endpoint is set *)
  if Config.websub_endpoint = "" then Lwt.return true else
  (* Do the WebSub ping *)
  let params = ["hub.mode",["publish"]; "hub.url",[updated]] in
  Client.post_form ~params Config.websub_endpoint >>= fun (resp, body) ->
    (* TODO log the ping with result *)
    let code =
      resp 
      |> Response.status 
      |> Code.code_of_status
    in
    let out = if code = 204 then true else false in
    out |> Lwt.return
