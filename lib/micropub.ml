open Lwt
open Opium.Std
open Config
open Entry
open Utils

(* OAuth errors *)
exception Error_insufficient_scope of string

exception Error_invalid_request of string

exception Error_not_found of string

let source_of_uri uri =
  let port =
    match uri |> Uri.port with Some p -> ":" ^ string_of_int p | None -> ""
  in
  match uri |> Uri.host with Some h -> h ^ port | None -> ""


(* Micropub GET handler *)
let micropub_query req =
  let headers = Cohttp.Header.init () |> set_content_type "application/json" in
  let uri = req |> Request.uri in
  let q = unwrap_option (Uri.get_query_param uri "q") "" in
  let url = unwrap_option (Uri.get_query_param uri "url") "" in
  if q = "config"
  then
    `Json
      (`O
        [ ( "post-types"
          , `A [ `O [ ("type", `String "article"); ("name", `String "Entry") ] ]
          )
        ])
    |> respond' ~headers
  else if (* TODO syndicate to a microblog.pub instance? *)
          q = "syndicate-to"
  then `Json (`O [ ("syndicate-to", `A []) ]) |> respond' ~headers
  else if q = "source"
  then
    if url = ""
    then raise (Error_invalid_request "missing url")
    else
      (* First try to see if a special mp-host entry is requested (with the host as uid) *)
      let headers = Cohttp.Request.headers req.request in
      let uri = url |> Uri.of_string in
      let rhost = source_of_uri uri in
      let uid, slug =
        try get_uid_and_slug (uri |> Uri.path) with _ -> ("", "")
      in
      Entry.get_by_uid_or_host uid rhost
      >>= fun some_stored ->
      match some_stored with
      | Some stored ->
          `Json stored |> respond' ~headers
      | None ->
          `Json (`O [ ("error", `String "url not found") ])
          |> respond' ~code:`Not_found ~headers
  else `Json (`O []) |> respond' ~headers


(* Micropub delete action handler *)
let micropub_delete url =
  if url = ""
  then raise (Error_invalid_request "missing url")
  else
    let uri = url |> Uri.of_string in
    let rhost = source_of_uri uri in
    let uid, slug =
      try get_uid_and_slug (uri |> Uri.path) with _ -> ("", "")
    in
    Entry.get_by_uid_or_host uid rhost
    >>= fun some_stored ->
    match some_stored with
    | Some stored ->
        let content = jform_field stored [ "properties"; "content" ] "" in
        let url = jform_field stored [ "properties"; "url" ] "" in
        Entry.remove uid
        >>= fun _ ->
        (* Entry is deleted at this point *)
        Entry.update_hook url content
        >>= fun _ -> `String "" |> respond' ~code:`No_content
    | None ->
        raise (Error_not_found "url not found")


let micropub_update_delete current_data jdata =
  if not Ezjsonm.(mem jdata [ "delete" ])
  then Ezjsonm.(get_dict current_data)
  else
    let props = Ezjsonm.(get_dict (find current_data [ "properties" ])) in
    let deleted = Ezjsonm.(find jdata [ "delete" ]) in
    let new_props =
      match deleted with
      | `O d ->
          List.fold_left
            (fun acc (k, v) ->
              let current =
                Ezjsonm.(get_strings (find current_data [ "properties"; k ]))
              in
              let to_delete = Ezjsonm.(get_strings v) in
              let new_data =
                current |> List.filter (fun x -> not (List.mem x to_delete))
              in
              (* TODO empty array instead of removing completely *)
              let nl = List.remove_assoc k acc in
              if List.length new_data > 0
              then nl @ [ (k, Ezjsonm.(strings new_data)) ]
              else nl)
            props
            d
      | `A ks ->
          List.fold_left
            (fun acc x -> List.remove_assoc x acc)
            props
            Ezjsonm.(get_strings deleted)
      | _ ->
          failwith "invalid delete payload"
    in
    Ezjsonm.(get_dict current_data)
    |> List.remove_assoc "properties"
    |> List.append [ ("properties", `O new_props) ]


let micropub_update_add current_data jdata =
  if not Ezjsonm.(mem jdata [ "add" ])
  then Ezjsonm.(get_dict current_data)
  else
    let props = Ezjsonm.(get_dict (find current_data [ "properties" ])) in
    let added = Ezjsonm.(get_dict (find jdata [ "add" ])) in
    let new_props =
      List.fold_left
        (fun acc (k, v) ->
          if not Ezjsonm.(mem current_data [ "properties"; k ])
          then acc @ [ (k, v) ]
          else
            let current =
              Ezjsonm.(get_strings (find current_data [ "properties"; k ]))
            in
            let new_v =
              (current @ Ezjsonm.(get_strings v))
              |> List.map (fun x -> `String x)
            in
            let nl = List.remove_assoc k acc in
            nl @ [ (k, `A new_v) ])
        props
        added
    in
    Ezjsonm.(get_dict current_data)
    |> List.remove_assoc "properties"
    |> List.append [ ("properties", `O new_props) ]


let micropub_update_replace current_data jdata =
  if not Ezjsonm.(mem jdata [ "replace" ])
  then Ezjsonm.(get_dict current_data)
  else
    let props = Ezjsonm.(get_dict (find current_data [ "properties" ])) in
    let replaced = Ezjsonm.(get_dict (find jdata [ "replace" ])) in
    let new_props =
      List.fold_left
        (fun acc (k, v) ->
          let nl = List.remove_assoc k acc in
          nl @ [ (k, v) ])
        props
        replaced
    in
    Ezjsonm.(get_dict current_data)
    |> List.remove_assoc "properties"
    |> List.append [ ("properties", `O new_props) ]


let micropub_update url jdata =
  if url = ""
  then raise (Error_invalid_request "missing url")
  else
    let uri = url |> Uri.of_string in
    let rhost = source_of_uri uri in
    let uid, slug =
      try get_uid_and_slug (uri |> Uri.path) with _ -> ("", "")
    in
    Entry.get_by_uid_or_host uid rhost
    >>= fun some_stored ->
    match some_stored with
    | Some stored ->
        let prev_url = jform_field stored [ "properties"; "url" ] "" in
        let prev_uid = jform_field stored [ "properties"; "uid" ] "" in
        let prev_content = jform_field stored [ "properties"; "content" ] "" in
        let doc_with_replace = micropub_update_replace stored jdata in
        let doc_with_delete =
          micropub_update_delete (`O doc_with_replace) jdata
        in
        let last_doc = micropub_update_add (`O doc_with_delete) jdata in
        (* Update the `updated` field *)
        let props =
          Ezjsonm.(get_dict (find (`O last_doc) [ "properties" ]))
          |> List.remove_assoc "updated"
          |> List.append
               [ ( "updated"
                 , `A [ `String (Datetime.now () |> Datetime.to_string) ] )
               ]
        in
        let final_doc =
          last_doc
          |> List.remove_assoc "properties"
          |> List.append [ ("properties", `O props) ]
        in
        let slug = jform_field stored [ "properties"; "mp-slug" ] "" in
        (* TODO ensure the field is set/jform_field variation that raise an error *)
        let old_and_new_content =
          prev_content
          ^ "\n"
          ^ jform_field (`O final_doc) [ "properties"; "content" ] ""
        in
        Entry.set prev_uid (`O final_doc)
        >>= fun _ ->
        (* Send both the old and new content for Webmention notification *)
        Entry.update_hook prev_url old_and_new_content
        >>= fun _ ->
        let headers = Cohttp.Header.init () |> add_header "Location" prev_url in
        `Json (`O final_doc) |> respond' ~code:`Created ~headers
    (* TODO return server_error from util *)
    | None ->
        raise (Error_not_found "url not found")


let with_default v default = if List.length v > 0 then v else default

(* Micropub JSON handler *)
let handle_json_create jdata scopes =
  (* Handle actions *)
  let action = jdata_field jdata [ "action" ] "" in
  let url = jdata_field jdata [ "url" ] "" in
  if action = "delete"
  then
    (* Ensure the delete scope is there *)
    if List.mem action scopes
    then micropub_delete url
    else raise (Error_insufficient_scope "missing \"delete\" scope")
  else if action = "update"
  then
    (* Ensure the delete scope is there *)
    if List.mem action scopes
    then micropub_update url jdata
    else raise (Error_insufficient_scope "missing \"update\" scope")
  else if (* Continue to process the entry creation, start by checking the create scope *)
          not (List.mem "create" scopes)
  then raise (Error_insufficient_scope "missing \"create\" scope")
  else
    let raw_entry = EntryModel.of_json jdata in
    let entry = EntryModel.with_defaults raw_entry in
    if List.hd entry.type_ <> "h-entry"
    then raise (Error_invalid_request "invalid type, only entry is supported")
    else
      save (get_prop_exn entry.properties.uid) entry
      >>= fun _ ->
      Entry.update_hook
        (get_prop_exn entry.properties.url)
        (get_prop_exn entry.properties.content)
      >>= fun _ ->
      let headers =
        Cohttp.Header.init ()
        |> add_header "Location" (List.hd entry.properties.url)
      in
      `String "" |> respond' ~code:`Created ~headers


(* combime all the "category" fields into one *)
let parse_cat dat =
  List.fold_left
    (fun acc (k, v) -> if k = "category" then acc @ v else acc)
    []
    dat


(* Micropub form handler *)
let handle_form_create dat scopes =
  (* Handle actions *)
  let action = form_value dat "action" "" in
  let url = form_value dat "url" "" in
  (* Is this a delete action? (update is unsupported via "form" request *)
  if action = "delete"
  then
    (* Ensure the delete scope is there *)
    if List.mem "delete" scopes
    then (* FIXME continue here *)
      micropub_delete url
    else raise (Error_insufficient_scope "missing \"delete\" scope")
  else if (* Continue to process the entry creation and check the create scope *)
          not (List.mem "create" scopes)
  then raise (Error_insufficient_scope "missing \"create\" scope")
  else
    let entry_type = "h-" ^ form_value dat "h" "entry" in
    let entry = EntryModel.of_form dat in
    if List.length entry.properties.content = 0
    then raise (Error_invalid_request "missing content")
    else if List.length entry.properties.name = 0
    then raise (Error_invalid_request "missing name")
    else if entry_type <> "entry"
    then raise (Error_invalid_request "invalid type, only entry is supported")
    else
      let new_entry = EntryModel.with_defaults entry in
      save (get_prop_exn new_entry.properties.uid) new_entry
      >>= fun _ ->
      Entry.update_hook
        (List.hd new_entry.properties.url)
        (List.hd new_entry.properties.content)
      >>= fun _ ->
      let headers =
        Cohttp.Header.init ()
        |> add_header "Location" (get_prop_exn new_entry.properties.url)
      in
      `String "" |> respond' ~code:`Created ~headers
