open Entriespub.Util

let test_get_uid_and_slug = [
  "get_uid_and_slug", `Quick, (fun () -> 
    let (uid, slug) = get_uid_and_slug "/uid/slug" in
    Alcotest.(check string) "check uid" "uid" uid;
    Alcotest.(check string) "check slug" "slug" slug
  );
  "get_uid_and_slug_failure", `Quick, (fun () ->
    let exn = Failure("bad URL") in
    Alcotest.check_raises "check bad path" exn (fun() -> let (uid, slug) = get_uid_and_slug "lol" in ())
  );
]

let test_suite () =
  Alcotest.run ~and_exit:true "lib" ["get_uid_and_slug", test_get_uid_and_slug]

let () =
  test_suite ()
