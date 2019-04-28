
(* Helper to parse an entry path (/{uid}/{slug}) *)
let get_uid_and_slug path =
  let dat = Str.split (Str.regexp "/") path in
  if List.length dat <> 2 then
    failwith "bad URL"
  else
    (List.nth dat 0, List.nth dat 1)

