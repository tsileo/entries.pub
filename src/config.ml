open Lwt.Infix
open Lwt
open Yurt
include Cohttp_lwt_unix.Server

module Store = Irmin_unix.Git.FS.KV(Irmin.Contents.String)

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

(* Irmin config *)
let config = Irmin_git.config ~bare:false "./db"

let author = "entries.pub <dev@entries.pub>"
 
let info fmt = Irmin_unix.info ~author fmt

(* Read the file as a string *)
let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = String.create n in
  really_input ic s 0 n;
  close_in ic;
  (s)

let html_tpl =
  load_file "template.html"
  |> Mustache.of_string

let atom_tpl =
  load_file "atom.xml"
  |> Mustache.of_string

(* TODO uses Ezjsonm.find instead *)
let find l key =
  match l with
  | `O list -> List.assoc key list
  | _ -> assert false

(* Micropub API error *)
let invalid_request_error desc = 
  `O [
    "error",  `String "invalid_request";
    "error_description", `String desc;
  ]

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

let rconf = match (load_file "config.yaml" |> Yaml.of_string) with
  | Ok r -> r
  | Error e -> failwith "failed to load config"

let base_url = jdata_field rconf ["base_url"] "http://localhost:7888"
let blog_name = jdata_field rconf ["blog_name"] "Untitled"
let author_name = jdata_field rconf ["author_name"] "Dev"
let author_email = jdata_field rconf ["author_email"] "dev@entries.pub"
let websub_endpoint = jdata_field rconf ["websub_endpoint"] ""

let add_headers h =
   h
   |> fun h -> Header.add h "Content-Type" "text/html; charset=utf-8"
   |> fun h -> Header.add h "X-Powered-By" "entries.pub"
   |> fun h -> Header.add h "Link" ("<" ^ base_url ^ "/micropub>; rel=\"micropub\",<" ^ base_url ^ "/webmention>; rel=\"webmention\"")
