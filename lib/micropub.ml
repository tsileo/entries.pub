open Lwt
open Yurt

open Config
open Entry
open Utils

(* Micropub GET handler *)
let micropub_query req =
  let headers = Header.init ()
  |> set_content_type "application/json" in
  let q = Yurt_util.unwrap_option_default (Query.string req "q") "" in
  let url = Yurt_util.unwrap_option_default (Query.string req "url") "" in
  if q = "config" then Server.json (`O []) ~headers else
  (* TODO syndacate to a microblog.pub instance? *)
  if q = "syndicate-to" then Server.json (`O ["syndicate-to", `A []]) ~headers else
  if q = "source" then begin
  if url = "" then Server.json (invalid_request_error "missing url") ~status:400 ~headers else
  let (uid, slug) = get_uid_and_slug (url |> Uri.of_string |> Uri.path) in 
  Entry.get uid >>= fun some_stored ->
    match some_stored with
    | Some stored ->
      let current_data = Ezjsonm.(from_string stored) in
      Server.json current_data ~headers
    | None ->
      Server.json (`O ["error", `String "url not found"]) ~status:404 ~headers
  end else
  Server.json (`O []) ~headers

(* Micropub delete action handler *)
let micropub_delete url =
  if url = "" then Server.json (invalid_request_error "missing url") ~status:400 else
  let (uid, slug) = get_uid_and_slug (url |> Uri.of_string |> Uri.path) in 
  Entry.get uid >>= fun some_stored ->
  match some_stored with
  | Some stored ->
    Entry.remove uid >>= fun () ->
    Websub.ping (base_url ^ "atom.xml") >>= fun (_) ->
      Server.string "" ~status:204
  | None -> Server.json (`O ["error", `String "url not found"]) ~status:404


let micropub_update_delete current_data jdata =
  if not Ezjsonm.(mem jdata ["delete"]) then Ezjsonm.(get_dict current_data) else
  let props = Ezjsonm.(get_dict (find current_data ["properties"])) in
  let deleted = Ezjsonm.(find jdata ["delete"]) in
  let new_props = match deleted with
  | `O d ->
    List.fold_left (fun acc x ->
      let (k, v) = x in
      let current = Ezjsonm.(get_strings (find current_data ["properties"; k])) in
      let to_delete = Ezjsonm.(get_strings v) in
      let new_data =
        current
        |> List.filter (fun x -> not (List.mem x to_delete))
      in
      (* TODO empty array instead of removing completely *)
      let nl = List.remove_assoc k acc in
      if List.length new_data > 0 then
        nl @ [k, Ezjsonm.(strings new_data)]
      else
        nl
    ) props d
  | `A ks ->
    List.fold_left (fun acc x ->
      List.remove_assoc x acc
    ) props Ezjsonm.(get_strings deleted)
  | _ -> failwith "lol" in
  Ezjsonm.(get_dict current_data)
  |> List.remove_assoc "properties"
  |> List.append ["properties", `O new_props]


let micropub_update_add current_data jdata =
  if not Ezjsonm.(mem jdata ["add"]) then Ezjsonm.(get_dict current_data) else
  let props = Ezjsonm.(get_dict (find current_data ["properties"])) in
  let added = Ezjsonm.(get_dict (find jdata ["add"])) in
  let new_props = List.fold_left (fun acc (k, v) ->
    if not Ezjsonm.(mem current_data ["properties"; k]) then
      acc @ [k, v]
    else
    let current = Ezjsonm.(get_strings (find current_data ["properties"; k])) in
    let new_v =
      current @ Ezjsonm.(get_strings v)
      |> List.map (fun x -> `String x)
     in
     let nl = List.remove_assoc k acc in
     nl @ [k, `A new_v]
  ) props added in
  Ezjsonm.(get_dict current_data)
  |> List.remove_assoc "properties"
  |> List.append ["properties", `O new_props]


let micropub_update_replace current_data jdata =
  if not Ezjsonm.(mem jdata ["replace"]) then Ezjsonm.(get_dict current_data) else
  let props = Ezjsonm.(get_dict (find current_data ["properties"])) in
  let replaced = Ezjsonm.(get_dict (find jdata ["replace"])) in
  let new_props = List.fold_left (fun acc (k, v) ->
    let nl = List.remove_assoc k acc in
    nl @ [k, v]
  ) props replaced in
  Ezjsonm.(get_dict current_data)
  |> List.remove_assoc "properties"
  |> List.append ["properties", `O new_props]


let micropub_update url jdata =
  Log.info "URL=%s" (Uri.of_string url |> Uri.path);
  if url = "" then Server.json (invalid_request_error "missing url") ~status:400 else
  let (uid, slug) = get_uid_and_slug (url |> Uri.of_string |> Uri.path) in 
  Entry.get uid >>= fun some_stored ->
  match some_stored with
  | Some stored ->
    let current_data = Ezjsonm.(from_string stored) in
    let doc_with_replace = micropub_update_replace current_data jdata in
    let doc_with_delete = micropub_update_delete (`O doc_with_replace) jdata in
    let last_doc = micropub_update_add (`O doc_with_delete) jdata in
    let slug = slugify (jform_field (`O last_doc) ["properties"; "name"] "") in
    if slug = "" then failwith "no slug" else
    let js = Ezjsonm.(to_string (`O last_doc)) in
    Entry.set uid js >>= fun () ->
    Websub.ping (base_url ^ "atom.xml") >>= fun (_) ->
      let headers = Header.init ()
      |> fun h -> Header.add h "Location" (build_url uid slug)
      in
      Server.json ~status:201 ~headers (`O last_doc)
  | None -> Server.json (`O ["error", `String "url not found"]) ~status:404

(* Micropub JSON handler *)
let handle_json_create body =
  Body.to_string body >>= fun sbody ->
    let jdata = Ezjsonm.from_string sbody in
    (* Handle actions *)
    let action = jdata_field jdata ["action"] "" in
    let url = jdata_field jdata ["url"] "" in
    if action = "delete" then micropub_delete url else
    if action = "update" then micropub_update url jdata else
    (* Continue to process the entry creation *)
    let entry_type = jform_field jdata ["type"] "" in
    let entry_content = jform_field jdata ["properties"; "content"] "" in
    let entry_name = jform_field jdata ["properties"; "name"] "Untitled" in
    let entry_category = jform_strings jdata ["properties"; "category"] in
    let entry_published = jform_field jdata ["properties"; "published"] (Date.now () |> Date.to_string) in
    if entry_type <> "h-entry" then
      Server.json (invalid_request_error "invalid type, only entry is supported") ~status:400
    else 
      let slug = slugify entry_name in
      let uid = new_id () in
      save uid slug entry_type entry_content entry_name entry_published entry_category >>= fun () ->
      Websub.ping (base_url ^ "atom.xml") >>= fun (_) ->
      let headers = Header.init ()
       |> fun h -> Header.add h "Location" (build_url uid slug) in
       Server.string "" ~status:201 ~headers

(* combime all the "category[]" field into one *)
let parse_cat dat =
  (List.fold_left (fun acc (k, v) ->
    if k = "category[]" then
      acc @ [Ezjsonm.(get_strings v) |> List.hd]
    else
      acc
  ) [] Ezjsonm.(get_dict dat))

(* Micropub form handler *)
let handle_form_create body =
  Form.urlencoded_json body >>= fun p ->
    Log.info "%s" Ezjsonm.(to_string p);
    let jdata = Ezjsonm.(value p) in
    (* Handle actions *)
    let action = jform_field jdata ["action"] "" in
    let url = jform_field jdata ["url"] "" in
    if action = "delete" then micropub_delete url else
    (* Continue to process the entry creation *)
    let entry_type = jform_field jdata ["h"] "entry" in
    let entry_content = jform_field jdata ["content"] "" in
    let entry_name = jform_field jdata ["name"] "Untitled" in
    let entry_category = parse_cat jdata in
    let entry_published = jform_field jdata ["published"] (Date.now () |> Date.to_string) in
    if entry_content = "" then
      Server.json (invalid_request_error "missing content") ~status:400
    else if entry_name = "" then
      Server.json (invalid_request_error "missing name") ~status:400
    else if entry_type <> "entry" then
      Server.json (invalid_request_error "invalid type, only entry is supported") ~status:400
    else
      let slug = slugify entry_name in
      let uid = new_id () in
      save uid slug ("h-" ^ entry_type) entry_content entry_name entry_published entry_category >>= fun () ->
      Websub.ping (base_url ^ "atom.xml") >>= fun (_) ->
      let headers = Header.init ()
       |> fun h -> Header.add h "Location" (build_url uid slug) in
       Server.string "" ~status:201 ~headers
