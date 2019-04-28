open Yurt

(* Helper to parse an entry path (/{uid}/{slug}) *)
let get_uid_and_slug path =
  let dat = Str.split (Str.regexp "/") path in
  if List.length dat <> 2 then
    failwith "bad URL"
  else
    (List.nth dat 0, List.nth dat 1)

(* Helper for setting the Content-Type header *)
let text_html = "text/html; charset=utf-8"
let set_content_type content_type h =
  h
  |> fun h -> Header.add h "Content-Type" content_type
  |> fun h -> Header.add h "X-Powered-By" "entries.pub"

(* add the Micropub/Webmention links TODO Websub? *)
let add_links base_url h =
  h
  |> fun h -> Header.add h "Link" ("<" ^ base_url ^ "/micropub>; rel=\"micropub\",<" ^ base_url ^ "/webmention>; rel=\"webmention\"")

(* Micropub API error *)
let build_error code desc = 
  `O [
    "error",  `String code;
    "error_description", `String desc;
  ]

(* Micropub API error *)
let invalid_request_error desc = 
  build_error "invalid_request" desc

(* Return the first item of the given list or a data *)
let jform_field jdata k default =
  if Ezjsonm.(mem jdata k) then
    let items = Ezjsonm.(get_strings (find jdata k)) in
      match items with
        | [] -> default
        | t :: _ -> t
  else
    default

(* Return the first item of the given list or a data *)
let jdata_field jdata k default =
  if Ezjsonm.(mem jdata k) then
    Ezjsonm.(get_string (find jdata k))
  else
    default

(* Read the file as a string *)
let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = String.create n in
  really_input ic s 0 n;
  close_in ic;
  (s)

(* Date helper *)
module Date = struct
  module D = ODate.Unix
  (* iso format *)
  let format = "%FT%TZ"
  let parseR = match D.From.generate_parser format with
    | Some p -> p
    | None -> failwith "could not generate parser"
  let printer = match D.To.generate_printer format with
    | Some p -> p
    | None -> failwith "could not generate printer"
  let pformat = "%b %d, %Y"
  let pprinter = match D.To.generate_printer pformat with
    | Some p -> p
    | None -> failwith "could not generate pprinter"


  type t = D.t
  let now () = D.now ()
  let to_string d = D.To.string ~tz:ODate.UTC printer d
  let to_pretty d = D.To.string ~tz:ODate.UTC pprinter d
  let of_string s = D.From.string parseR s
end
