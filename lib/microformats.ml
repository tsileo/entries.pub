include Cohttp
include Cohttp.Link
include Soup

open Utils

(* Basic microformats2 parser for Webmention, returns JSON mf2 *)
let parse url soup target =
  let title = try
    soup $ "title" |> (fun node ->
      node |> R.leaf_text |> String.trim
    )
  with _ -> "" in
  let hcard_soup = try
    soup $ ".h-card"
  with _ -> 
    (* TODO support no hcard *)
    failwith "no hcard"
  in
  let photo = try
    hcard_soup $ ".u-photo" |> (fun node ->
      let name = node |> name in
      if name = "data" then
        node |> R.attribute "value"
      else
        node |> R.attribute "src"
    )
  with _ -> ""
  in
  let url = try
    hcard_soup $ ".u-url" |> (fun node ->
      node |> R.attribute "href" |> String.trim
    )
  with _ ->
    let u = Uri.of_string url in
    Uri.with_path u "/" |> Uri.to_string
  in
  let name = try
    hcard_soup $ ".p-name" |> (fun node ->
      node |> R.leaf_text |> String.trim
    )
  with _ -> Uri.of_string url |> Uri.host |> fun x -> Yurt_util.unwrap_option_default x ""
  in
  let like_of = try
    hcard_soup $ "a.u-like-of" |> (fun node ->
      node |> R.attribute "href" |> String.trim
    )
  with _ -> ""
  in
  let repost_of = try
    hcard_soup $ "a.u-repost-of" |> (fun node ->
      node |> R.attribute "href" |> String.trim
    )
  with _ -> ""
  in
  let in_reply_to = try
    hcard_soup $ "a.u-in-reply-to" |> (fun node ->
      node |> R.attribute "href" |> String.trim
    )
  with _ -> ""
  in
  let mtype = if like_of = target then
    "like"
  else if repost_of = target then
    "repost"
  else if in_reply_to = target then
     "reply"
  else
    "mention" 
  in
  `O [
    "author_photo", `String photo;
    "author_url", `String url;
    "author_name", `String name;
    "url", `String url;
    "type", `String mtype;
    "title", `String title;
  ]
