open Lwt.Infix
open Lwt
open Yurt
include Cohttp_lwt_unix.Server

open Config
open Entry

(* Micropub GET handler *)
let micropub_query req =
 (* TODO check auth and handle JSON *)
  let q = Yurt_util.unwrap_option_default (Query.string req "q") "" in
  let url = Yurt_util.unwrap_option_default (Query.string req "url") "" in
  if q = "config" then Server.json (`O []) else
  if q = "syndicate-to" then Server.json (`O ["syndicate-to", `A []]) else
  if q = "source" then begin
  if url = "" then
    (Server.json (invalid_request_error "url") ~status:400)
  else
  let slug = Uri.of_string url |> Uri.path |> path_to_slug in
  Store.Repo.v config >>=
  Store.master >>= fun t ->
  Store.find t ["entries"; slug] >>= fun some_stored ->
    match some_stored with
    | Some stored ->
      let current_data = Ezjsonm.(from_string stored) in
      Server.json current_data
    | None ->
      Server.json (`O ["error", `String "url not found"]) ~status:404
  end else
  Server.json (`O [])

(* Micropub delete action handler *)
let micropub_delete url =
  if url == "" then
    Server.json (invalid_request_error "url") ~status:400
  else
  let slug = Uri.of_string url |> Uri.path |> path_to_slug in
  Store.Repo.v config >>=
  Store.master >>= fun t ->
  Store.find t ["entries"; slug] >>= fun some_stored ->
  match some_stored with
  | Some stored ->
    Store.remove t ~info:(info "Deleting an entry") ["entries"; slug] >>= fun () ->
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
        |> List.map (fun x -> `String x)
      in
      let nl = List.remove_assoc k acc in
      if List.length new_data > 0 then
        nl @ [k, (`A new_data)]
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
  if url = "" then Server.json (invalid_request_error "url") ~status:400 else
  let slug = Uri.of_string url |> Uri.path |> path_to_slug in
  Store.Repo.v config >>=
  Store.master >>= fun t ->
  Store.find t ["entries"; slug] >>= fun some_stored ->
  match some_stored with
  | Some stored ->
    let current_data = Ezjsonm.(from_string stored) in
    let doc_with_replace = micropub_update_replace current_data jdata in
    let doc_with_delete = micropub_update_delete (`O doc_with_replace) jdata in
    let last_doc = micropub_update_add (`O doc_with_delete) jdata in
    let slug = slugify (jform_field (`O last_doc) ["properties"; "name"] "") in
    if slug = "" then failwith "no slug" else
    let js = Ezjsonm.(to_string (`O last_doc)) in
    Store.set t ~info:(info "Updating an entry") ["entries"; slug] js >>= fun () ->
      let headers = Header.init ()
      |> fun h -> Header.add h "Location" (base_url ^ "/" ^ slug)
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
    (* TODO handle entry creation *)
    Server.string entry_type


(* Micropub form handler *)
let handle_form_create body =
  Form.urlencoded_json body >>= fun p ->
    let jdata = Ezjsonm.(value p) in
    (* Handle actions *)
    let action = jform_field jdata ["action"] "" in
    let url = jform_field jdata ["url"] "" in
    if action = "delete" then micropub_delete url else
    (* Continue to process the entry creation *)
    let entry_type = jform_field jdata ["h"] "entry" in
    let entry_content = jform_field jdata ["content"] "" in
    let entry_name = jform_field jdata ["name"] "" in
    let entry_published = jform_field jdata ["published"] (Date.now () |> Date.to_string) in
    if entry_content = "" then
      Server.json (invalid_request_error "missing content") ~status:400
    else if entry_name = "" then
      Server.json (invalid_request_error "missing name") ~status:400
    else if entry_published = "" then
      Server.json (invalid_request_error "invalid published") ~status:400
    else if entry_type <> "entry" then
      Server.json (invalid_request_error "invalid type, only entry is supported") ~status:400
    else
      let obj = `O [
        "type", `A [ `String ("h-" ^ entry_type) ];
        "properties", `O [
          "content", `A [ `String entry_content ];
          "name", `A [ `String entry_name ];
          "published", `A [ `String entry_published ];
        ]
      ] in
      let slug = slugify entry_name in
      let js = Ezjsonm.to_string obj in
      Store.Repo.v config >>=
      Store.master >>= fun t ->
        Store.set t ~info:(info "Creating a new entry") ["entries"; slug] js >>= fun () ->
          let headers = Header.init ()
           |> fun h -> Header.add h "Location" (base_url ^ "/" ^ slug) in
          Server.string "" ~status:201 ~headers
