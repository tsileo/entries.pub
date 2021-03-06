open Lwt.Infix
open Opium.Std
open Stdint

let source_of_uri uri =
  let port =
    match uri |> Uri.port with Some p -> ":" ^ string_of_int p | None -> ""
  in
  match uri |> Uri.host with Some h -> h ^ port | None -> ""


let form_value dict key default =
  try List.assoc key dict |> List.hd with Not_found -> default


let get_header headers k =
  match Cohttp.Header.(get headers k) with Some v -> v | _ -> ""


let client_get ?headers url =
  Cohttp_lwt_unix.Client.get ?headers Uri.(of_string url)
  >>= fun (resp, body) ->
  Cohttp_lwt.Body.to_string body
  >|= fun body_string ->
  (* Also return the status code as int for easy logging *)
  let code = resp |> Cohttp.Response.status |> Cohttp.Code.code_of_status in
  (resp, code, body_string)


let client_post_form ?headers ~params url =
  Cohttp_lwt_unix.Client.post_form ?headers ~params (Uri.of_string url)
  >>= fun (resp, body) ->
  Cohttp_lwt.Body.to_string body
  >|= fun body_string ->
  (* Also return the status code as int for easy logging *)
  let code = resp |> Cohttp.Response.status |> Cohttp.Code.code_of_status in
  (resp, code, body_string)


let unwrap_option opt default = match opt with Some v -> v | _ -> default

let encode_uint32 buf v =
  for i = 0 to 3 do
    let b =
      Uint32.logand Uint32.(of_int 255) (Uint32.shift_right v (24 - (i * 8)))
    in
    Bytes.set buf i (Char.chr (Uint32.to_int b))
  done


(* Generate a new ID (hex-encoded), like a MongoDB ObjectID, the 4 first bytes contains the timestamp *)
let new_id () =
  let fd = Unix.openfile "/dev/urandom" [ Unix.O_RDONLY ] 0o400 in
  let len = 8 in
  let buff = Bytes.create len in
  (* Unix timestamp encoded in big-endian to let us sort the entries from most recent for free *)
  encode_uint32 buff Uint32.(of_float (Unix.time ())) ;
  (* Append 4 random bytes *)
  Unix.read fd buff 4 4 ;
  Unix.close fd ;
  Hex.of_string (Bytes.unsafe_to_string buff) |> Hex.show


(* Helper to parse an entry path (/{uid}/{slug}) *)
let get_uid_and_slug path =
  let dat = Str.split (Str.regexp "/") path in
  if List.length dat <> 2
  then failwith "bad URL"
  else (List.nth dat 0, List.nth dat 1)


(* Helper for setting the Content-Type header *)
let text_html = "text/html; charset=utf-8"

let set_content_type content_type h =
  h
  |> fun h ->
  Cohttp.Header.add h "Content-Type" content_type
  |> fun h -> Cohttp.Header.add h "X-Powered-By" "entries.pub"


let add_header k v h = h |> fun h -> Cohttp.Header.add h k v

(* add the Micropub/Webmention links *)
let add_micropub_header base_url h =
  h |> add_header "Link" ("<" ^ base_url ^ "/micropub>; rel=\"micropub\"")


let add_webmention_header base_url h =
  h |> add_header "Link" ("<" ^ base_url ^ "/webmention>; rel=\"webmention\"")


(* Micropub API error *)
let build_error code desc =
  `O [ ("error", `String code); ("error_description", `String desc) ]


(* Micropub API error *)
let invalid_request_error desc = build_error "invalid_request" desc

(* Return the first item of the given list or a data *)
let jdata_field jdata k default =
  if Ezjsonm.(mem jdata k) then Ezjsonm.(get_string (find jdata k)) else default


let jdata_bool jdata k default =
  if Ezjsonm.(mem jdata k) then Ezjsonm.(get_bool (find jdata k)) else default


(* Return the first item of the given list or a data *)
let jform_field jdata k default =
  if Ezjsonm.(mem jdata k)
  then
    let items = Ezjsonm.(get_strings (find jdata k)) in
    match items with [] -> default | t :: _ -> t
  else default


(* Return the first item of the given list or a data *)
let jform_strings jdata k =
  if Ezjsonm.(mem jdata k) then Ezjsonm.(get_strings (find jdata k)) else []


(* Read the file as a string *)
let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = String.create n in
  really_input ic s 0 n ;
  close_in ic ;
  Bytes.unsafe_to_string s


(* Date helper *)
module Datetime = struct
  module C = CalendarLib.Calendar
  module P = CalendarLib.Printer.Calendar

  let now () = C.(now () |> to_gmt)

  let to_pretty = P.sprint "%b %d, %Y"

  let to_string = P.sprint "%Y-%m-%dT%TZ"

  let of_string = P.from_fstring "%Y-%m-%dT%TZ"
end
