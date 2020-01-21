open Lwt.Infix
open Lwt
include Cohttp_lwt_unix.Server
open Config
open Utils

(* Return the first item of the given list or a data *)
let json_prop_values jdata k =
  if Ezjsonm.(mem jdata k) then Ezjsonm.(get_strings (find jdata k)) else []


module Props = struct
  type t =
    { name : string list
    ; content : string list
    ; published : string list
    ; updated : string list
    ; uid : string list
    ; category : string list
    ; url : string list
    ; (* mp-slug extension *)
      slug : string list
    ; (* Custom ext *)
      extra_head : string list
    ; extra_body : string list
    ; host : string list
    }

  let of_json j =
    { name = json_prop_values j [ "properties"; "name" ]
    ; content = json_prop_values j [ "properties"; "content" ]
    ; published = json_prop_values j [ "properties"; "published" ]
    ; updated = json_prop_values j [ "properties"; "updated" ]
    ; uid = json_prop_values j [ "properties"; "uid" ]
    ; category = json_prop_values j [ "properties"; "category" ]
    ; url = json_prop_values j [ "properties"; "url" ]
    ; slug = json_prop_values j [ "properties"; "mp-slug" ]
    ; extra_head = json_prop_values j [ "properties"; "mp-extra-head" ]
    ; extra_body = json_prop_values j [ "properties"; "mp-extra-head" ]
    ; host = json_prop_values j [ "properties"; "mp-host" ]
    }


  let to_json d =
    [ ("name", Ezjsonm.strings d.name)
    ; ("content", Ezjsonm.strings d.content)
    ; ("published", Ezjsonm.strings d.published)
    ; ("updated", Ezjsonm.strings d.updated)
    ; ("uid", Ezjsonm.strings d.uid)
    ; ("category", Ezjsonm.strings d.category)
    ; ("url", Ezjsonm.strings d.url)
    ; ("mp-slug", Ezjsonm.strings d.slug)
    ; ("mp-extra-head", Ezjsonm.strings d.extra_head)
    ; ("mp-extra-body", Ezjsonm.strings d.extra_body)
    ; ("mp-host", Ezjsonm.strings d.host)
    ]
end

let with_default vs default =
  if List.length vs > 0 && List.hd vs <> "" then vs else default


let get_prop_exn vs = List.hd vs

let get_prop vs default = try List.hd vs with _ -> default

(* Slugify replaces whitespaces by dashes, lowecases and remove any non alphanum chars. *)
let slugify k =
  k
  |> Str.global_replace (Str.regexp " ") "-"
  |> String.lowercase_ascii
  |> Str.global_replace (Str.regexp "[^a-z0-9\\-]") ""


(* combime all the "category" fields into one *)
let parse_cat dat =
  List.fold_left
    (fun acc (k, v) -> if k = "category" then acc @ v else acc)
    []
    dat


module EntryModel = struct
  type t =
    { type_ : string list
    ; properties : Props.t
    }

  let to_json d =
    `O
      [ ("type", Ezjsonm.strings d.type_)
      ; ("properties", `O (Props.to_json d.properties))
      ]


  let of_json j =
    { type_ = json_prop_values j [ "type" ]; properties = Props.of_json j }


  let of_form dat =
    { type_ = [ "h-entry" ]
    ; properties =
        { content = List.assoc "content" dat
        ; name = List.assoc "name" dat
        ; slug = List.assoc "slug" dat
        ; extra_head = List.assoc "mp-extra-head" dat
        ; extra_body = List.assoc "mp-extra-body" dat
        ; host = List.assoc "mp-host" dat
        ; url = []
        ; category = []
        ; (* FIXME: fix cats *)
          uid = []
        ; updated = []
        ; published = []
        }
    }


  let with_defaults raw_entry =
    let raw_props = raw_entry.properties in
    let is_host = if List.length raw_props.host > 0 then true else false in
    let uid = if is_host then raw_props.host else [ new_id () ] in
    let slug =
      with_default raw_props.slug [ slugify (List.hd raw_props.name) ]
    in
    { raw_entry with
      properties =
        { raw_props with
          published =
            with_default
              raw_props.published
              [ Datetime.now () |> Datetime.to_string ]
        ; updated =
            with_default
              raw_props.updated
              [ Datetime.now () |> Datetime.to_string ]
        ; url =
            [ ( if is_host
              then "https://" ^ List.hd uid
              else build_url (List.hd uid) (List.hd slug) )
            ]
        ; slug
        ; uid
        }
    }


  let to_tpl entry =
    let updated = get_prop entry.properties.updated "" in
    let updated_pretty =
      if updated = ""
      then ""
      else Datetime.of_string updated |> Datetime.to_pretty
    in
    let published = get_prop_exn entry.properties.published in
    let has_category =
      if List.length entry.properties.category > 0 then true else false
    in
    let is_page =
      if List.mem "page" entry.properties.category then true else false
    in
    let is_draft =
      if List.mem "draft" entry.properties.category then true else false
    in
    let has_been_updated =
      if updated = ""
      then false
      else if published <> updated
      then true
      else false
    in
    let is_host =
      if get_prop entry.properties.host "" <> "" then true else false
    in
    `O
      [ ("name", `String (get_prop_exn entry.properties.name))
      ; ("slug", `String (get_prop_exn entry.properties.slug))
      ; ( "content"
        , `String
            ( get_prop_exn entry.properties.content
            |> Omd.of_string
            |> Omd.to_html ) )
      ; ("published", `String published)
      ; ( "published_pretty"
        , `String (published |> Datetime.of_string |> Datetime.to_pretty) )
      ; ("updated", `String updated)
      ; ("updated_pretty", `String updated_pretty)
      ; ("author_name", `String author_name)
      ; ("author_email", `String author_email)
      ; ("author_url", `String base_url)
      ; ("author_icon", `String author_icon)
      ; ("url", `String (get_prop_exn entry.properties.url))
      ; ("uid", `String (get_prop_exn entry.properties.uid))
      ; ("category", Ezjsonm.(strings entry.properties.category))
      ; ("has_category", `Bool has_category)
      ; ("extra_head", `String (get_prop entry.properties.extra_head ""))
      ; ("extra_body", `String (get_prop entry.properties.extra_body ""))
      ; ("is_draft", `Bool is_draft)
      ; ("is_page", `Bool is_page)
      ; ("is_host", `Bool is_host)
      ; ("host", `String (get_prop entry.properties.host ""))
      ; ("has_been_updated", `Bool has_been_updated)
      ]
end

let discard_pages_and_drafts entries =
  Lwt_list.filter_s
    (fun d ->
      (not
         (jdata_bool d [ "is_page" ] false or jdata_bool d [ "is_draft" ] false))
      |> Lwt.return)
    entries


(* Iter over all the entries as JSON objects *)
let iter map =
  Store.Repo.v config
  >>= Store.master
  >>= fun t ->
  Store.list t [ "entries" ]
  >>= fun keys ->
  (* Rev map for getting more recents post first *)
  Lwt_list.rev_map_s
    (fun (s, c) ->
      Store.get t [ "entries"; s ]
      >>= fun stored -> stored |> Ezjsonm.from_string |> map |> Lwt.return)
    keys


(* Get a specific entry as JSON *)
let get uid =
  Store.Repo.v config
  >>= Store.master
  >>= fun t ->
  Store.find t [ "entries"; uid ]
  >>= fun entry ->
  match entry with
  | Some e ->
      (* Render the entry *)
      let v = e |> Ezjsonm.from_string in
      Some v |> Lwt.return
  | None ->
      Lwt.return None


let get_by_uid_or_host uid host =
  Store.Repo.v config
  >>= Store.master
  >>= fun t ->
  Store.find t [ "entries"; host ]
  >>= fun entry ->
  match entry with
  | Some e ->
      (* Render the entry *)
      let v = e |> Ezjsonm.from_string in
      Some v |> Lwt.return
  | None ->
      Store.find t [ "entries"; uid ]
      >>= fun entry ->
      ( match entry with
      | Some e ->
          (* Render the entry *)
          let v = e |> Ezjsonm.from_string in
          Some v |> Lwt.return
      | None ->
          Lwt.return None )


(* Remove the given entry *)
let remove uid =
  Store.Repo.v config
  >>= Store.master
  >>= fun t ->
  Store.remove t ~info:(info "Deleting entry %s" uid) [ "entries"; uid ]


(* Set/update an entry *)
let set uid entry =
  Store.Repo.v config
  >>= Store.master
  >>= fun t ->
  Store.set
    t
    ~info:(info "Updating entry %s" uid)
    [ "entries"; uid ]
    Ezjsonm.(to_string entry)


let save uid entry = set uid (EntryModel.to_json entry)

let update_hook url body =
  Websub.ping Config.base_url
  >>= fun _ ->
  let hbody = Omd.of_string body |> Omd.to_html in
  Webmention.send_webmentions url hbody >|= fun _ -> true
