open Yurt

(* Helper to parse an entry path (/{uid}/{slug}) *)
let get_uid_and_slug path =
  let dat = Str.split (Str.regexp "/") path in
  if List.length dat <> 2 then
    failwith "bad URL"
  else
    (List.nth dat 0, List.nth dat 1)

let text_html = "text/html; charset=utf-8"

let set_content_type content_type h =
  h
  |> fun h -> Header.add h "Content-Type" content_type
  |> fun h -> Header.add h "X-Powered-By" "entries.pub"

let add_links base_url h =
  h
  |> fun h -> Header.add h "Link" ("<" ^ base_url ^ "/micropub>; rel=\"micropub\",<" ^ base_url ^ "/webmention>; rel=\"webmention\"")
