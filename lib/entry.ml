open Lwt.Infix
open Lwt
open Yurt
include Cohttp_lwt_unix.Server
open Config
open Util

(* Slugify replaces whitespaces by dashes, lowecases and remove any non alphanum chars. *)
let slugify k =
  k
  |> Str.global_replace (Str.regexp " ") "-"
  |> String.lowercase_ascii
  |> Str.global_replace (Str.regexp "[^a-z0-9\\-]") ""

(* Helper for sorting entrie by date *)
let compare_entry_data a b =
  let a_published = jdata_field a ["published"] "" in
  let b_published = jdata_field b ["published"] "" in
  (String.compare b_published a_published)

(* Convert a JSON entry into template data *)
let entry_tpl_data jdata =
  let name = jform_field jdata ["properties"; "name"] "" in
  let content = jform_field jdata ["properties"; "content"] "" in
  let published = jform_field jdata ["properties"; "published"] "" in
  let uid = jform_field jdata ["properties"; "uid"] "" in
  let slug = slugify name in
  `O [
    "name", `String name;
    "slug", `String slug;
    "content", `String (Omd.of_string content |> Omd.to_html);
    "published", `String published;
    "published_pretty", `String (Date.of_string published |> Date.to_pretty);
    "author_name", `String author_name;
    "author_email", `String author_email;
    "url", `String (base_url ^ "/" ^ uid ^ "/" ^ slug);
    "uid", `String uid;
  ]

let path_to_slug str =
  if str = "" then "" else
  String.sub str 1 ((String.length str) - 1)

let iter () =
  Store.Repo.v config >>=
  Store.master >>= fun t ->
  Store.list t ["entries"] >>= fun keys ->
    (Lwt_list.map_s (fun (s, c) ->
      Store.get t ["entries"; s] >>= fun stored ->
        stored |> Ezjsonm.from_string |> entry_tpl_data |> Lwt.return
    ) keys)

let get uid =
  Store.Repo.v config >>=
  Store.master >>= fun t ->
    Store.find t ["entries"; uid]
