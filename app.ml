open Lwt.Infix
open Lwt
open Yurt
include Cohttp_lwt_unix.Server

module Store = Irmin_unix.Git.FS.KV(Irmin.Contents.String)

let slugify k =
  k
  |> Str.global_replace (Str.regexp " ") "_"
  |> String.lowercase_ascii
  |> Str.global_replace (Str.regexp "[^a-z0-9_]") ""

let config = Irmin_git.config ~bare:false "./db"

let author = "entries.pub <dev@entries.pub>"
 
let info fmt = Irmin_unix.info ~author fmt

let find l key =
  match l with
  | `O list -> List.assoc key list
  | _ -> assert false

(* Micropub API error *)
let invalid_request_error desc = 
  `O [ "error",  `String "invalid_request"; "error_description", `String desc]

(* Return the first item of the given list or a data *)
let jform_field jdata k default =
  if Ezjsonm.(mem jdata [k]) then
    let items = Ezjsonm.(get_strings (find jdata [k])) in
      match items with
        | [] -> default
        | t :: _ -> t
  else
    default

(* Create a new note via a POSTed form *)
let handle_form_create body =
  Form.urlencoded_json body >>= fun p ->
  let jdata = Ezjsonm.(value p) in
  let entry_type = jform_field jdata "h" "entry" in
  let entry_content = jform_field jdata "content" "" in
  let entry_name = jform_field jdata "name" "" in
  if entry_content = "" then
    Server.json (invalid_request_error "missing content") ~status:400
  else if entry_name = "" then
    Server.json (invalid_request_error "missing name") ~status:400
  else
    let obj = `O [
      "type", `A [ `String ("h-" ^ entry_type) ];
      "properties", `O [
        "content", `A [ `String entry_content ];
        "name", `A [ `String entry_name ]
      ]
    ] in
    Store.Repo.v config >>=
    Store.master >>= fun t ->
    Store.set t ~info:(info "Creating a new entry") ["entries"; slugify entry_name] Ezjsonm.(to_string obj) >>= fun () ->
      Server.string "ok"


let _ =
let open Server in

(* Create a server *)
server "127.0.0.1" 7888

>| get "/" (fun req params body ->
  Store.Repo.v config >>=
  Store.master >>= fun t ->
  Store.list t ["entries"] >>= fun keys ->
    Lwt_list.map_s (fun (s, c) ->
      Store.get t ["entries";s] >>= fun stored ->
        stored |> Ezjsonm.from_string |> Lwt.return
    ) keys >>= fun dat ->
    json (`O ["data", `A dat]))

>| post "/micropub" (fun req params body->
    handle_form_create body)

(* Run it *)
|> run
