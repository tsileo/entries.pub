open Lwt.Infix
open Lwt
include Cohttp_lwt_unix.Server

open Config
open Utils

(* Slugify replaces whitespaces by dashes, lowecases and remove any non alphanum chars. *)
let slugify k =
  k
  |> Str.global_replace (Str.regexp " ") "-"
  |> String.lowercase_ascii
  |> Str.global_replace (Str.regexp "[^a-z0-9\\-]") ""

(* Convert a JSON entry into template data *)
let entry_tpl_data jdata =
  let name = jform_field jdata ["properties"; "name"] "" in
  let content = jform_field jdata ["properties"; "content"] "" in
  let published = jform_field jdata ["properties"; "published"] "" in
  let updated = jform_field jdata ["properties"; "updated"] "" in
  let updated_pretty = if updated = "" then "" else (Date.of_string updated |> Date.to_pretty) in
  let uid = jform_field jdata ["properties"; "uid"] "" in
  let tags = jform_strings jdata ["properties"; "category"] in
  let slug = jform_field jdata ["properties"; "mp-slug"] (slugify name) in
  let has_category = if List.length tags > 0 then true else false in
  let is_page = if List.mem "page" tags then true else false in
  `O [
    "name", `String name;
    "slug", `String slug;
    "content", `String (Omd.of_string content |> Omd.to_html);
    "published", `String published;
    "published_pretty", `String (Date.of_string published |> Date.to_pretty);
    "updated", `String updated;
    "updated_pretty", `String updated_pretty;
    "author_name", `String author_name;
    "author_email", `String author_email;
    "author_url", `String base_url;
    "url", `String (base_url ^ "/" ^ uid ^ "/" ^ slug);
    "uid", `String uid;
    "category", Ezjsonm.(strings tags);
    "has_category", `Bool has_category;
    "is_page", `Bool is_page;
  ]

let discard_pages entries =
  Lwt_list.filter_s (fun d -> not (jdata_bool d ["is_page"] false) |> Lwt.return) entries

(* Iter over all the entries as JSON objects *)
let iter map =
  Store.Repo.v config >>=
  Store.master >>= fun t ->
  Store.list t ["entries"] >>= fun keys ->
    (* Rev map for getting more recents post first *)
    (Lwt_list.rev_map_s (fun (s, c) ->
      Store.get t ["entries"; s] >>= fun stored ->
        stored |> Ezjsonm.from_string |> map |> Lwt.return
    ) keys)

(* Get a specific entry as JSON *)
let get uid =
  Store.Repo.v config >>=
  Store.master >>= fun t ->
    Store.find t ["entries"; uid] >>= fun entry ->
    match entry with
    | Some e ->
      (* Render the entry *)
      let v = e |> Ezjsonm.from_string in
      Some v |> Lwt.return
   | None ->
      Lwt.return None

(* Remove the given entry *)
let remove uid =
  Store.Repo.v config >>=
  Store.master >>= fun t ->
    Store.remove t ~info:(info "Deleting entry %s" uid) ["entries"; uid]

(* Set/update an entry *)
let set uid entry =
  Store.Repo.v config >>=
  Store.master >>= fun t ->
  Store.set t ~info:(info "Updating entry %s" uid) ["entries"; uid] Ezjsonm.(to_string entry)
 
(* Save a new entry *)
let save uid slug entry_type entry_content entry_name entry_published entry_category =
  (* Serialize the entry to JSON microformats2 format *)
  let obj = `O [
    "type", `A [ `String entry_type ];
    "properties", `O [
      "content", `A [ `String entry_content ];
      "name", `A [ `String entry_name ];
      "published", `A [ `String entry_published ];
      "updated", `A [ `String entry_published ];
      "uid", `A [ `String uid ];
      "url", `A [ `String (build_url uid slug) ];
      "category", Ezjsonm.(strings entry_category);
      "mp-slug", `A [ `String slug ];
    ]
  ] in
  (* JSON serialize *)
  Log.info "%s" Ezjsonm.(to_string obj);
  (* Save to repo *)
  set uid obj

let update_hook url body =
  Websub.ping Config.base_url >>= fun _ ->
    let hbody = Omd.of_string body |> Omd.to_html in
    Webmention.send_webmentions url hbody >>= fun _ -> Lwt.return true
