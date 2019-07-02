open Lwt.Infix
open Lwt
open Utils
module Store = Irmin_unix.Git.FS.KV (Irmin.Contents.String)

let html_tpl = load_file "template.html" |> Mustache.of_string

let atom_tpl = load_file "atom.xml" |> Mustache.of_string

let rconf =
  match load_file "config.yaml" |> Yaml.of_string with
  | Ok r -> r
  | Error e -> failwith "failed to load config"

let base_url = jdata_field rconf ["base_url"] "http://localhost:7888"

let blog_name = jdata_field rconf ["blog_name"] "Untitled"

let hero = jdata_field rconf ["hero"] ""

let author_name = jdata_field rconf ["author_name"] "Dev"

let author_email = jdata_field rconf ["author_email"] "dev@entries.pub"

let author_icon = jdata_field rconf ["author_icon"] ""

let websub_endpoint = jdata_field rconf ["websub_endpoint"] ""

let token_endpoint = jdata_field rconf ["token_endpoint"] ""

let authorization_endpoint = jdata_field rconf ["authorization_endpoint"] ""

(* For Irmin (a la Git) *)
let author = author_email ^ " <" ^ author_email ^ ">"

(* Irmin config *)
let config = Irmin_git.config ~bare:false "./db"

let info fmt = Irmin_unix.info ~author fmt

let build_url uid slug = base_url ^ "/" ^ uid ^ "/" ^ slug
