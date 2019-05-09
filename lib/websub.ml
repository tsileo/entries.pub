open Lwt.Infix
open Cohttp
open Soup

open Utils

(* Notify the WebSub hub that a resource has been updated *)
let ping base_url =
  (* Ensure a WebSub endpoint is set *)
  if Config.websub_endpoint = "" then Lwt.return true else
  (* Do the WebSub ping *)
      let params = ["hub.mode",["publish"]; "hub.url",[base_url ^ "/atom.xml"]; "hub.url",[base_url ^ "/feed.json"]] in
  client_post_form ~params Config.websub_endpoint >|= fun (resp, code, body) ->
    (* TODO log the ping with result *)
    if code = 204 then true else false
