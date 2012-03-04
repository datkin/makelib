open Test
open Assert

let ident x = x

let string_tests =
  let open Util in
  [ make "prefix" (fun () ->
    is_true (String.is_prefix ~prefix:"foo" "foobar"))
  ; make "always prefix" (fun () ->
    is_true (String.is_prefix ~prefix:"" "foobar"))
  ; make "prefix full" (fun () ->
    is_true (String.is_prefix ~prefix:"foobar" "foobar"))
  ; make "not prefix extra" (fun () ->
    is_false (String.is_prefix ~prefix:"foobarx" "foobar"))
  ; make "not prefix" (fun () ->
    is_false (String.is_prefix ~prefix:"bar" "foobar"))
  ; make "suffix" (fun () ->
    is_true (String.is_suffix ~suffix:"bar" "foobar"))
  ; make "not suffix" (fun () ->
    is_false (String.is_suffix ~suffix:"foo" "foobar"))
  ; make "suffix full" (fun () ->
    is_true (String.is_suffix ~suffix:"foobar" "foobar"))
  ; make "split" (fun () ->
    list_eq ~to_string:ident [""; "a"; ""] (String.split ~on:' ' " a "))
  ; make "split 2" (fun () ->
    list_eq ~to_string:ident ["a"; "b"; "c"] (String.split ~on:' ' "a b c"))
  ; make "nosplit" (fun () ->
    expect (String.split ~on:' ' "foo") ~is:["foo"])
  ; make "lsplit" (fun () ->
    expect (String.lsplit2 "a,b,c" ~on:',') ~is:(Some ("a", "b,c")))
  ; make "no lsplit" (fun () ->
    expect (String.lsplit2 "foo" ~on:',') ~is:None)
  ; make "rsplit" (fun () ->
    expect (String.rsplit2 "a,b,c" ~on:',') ~is:(Some ("a,b", "c")))
  ; make "empty" (fun () ->
    (is_true (String.is_empty "")) <&&> (is_false (String.is_empty "x")))
  ]
;;

let len_equal x y =
  String.length x = String.length y
;;

let list_tests =
  let open Util in
  [ make "list string" (fun () ->
    string_eq "[a; b]" (List.to_string ["a"; "b"] ~to_string:ident))
  ; make "init" (fun () ->
    list_eq ~to_string:string_of_int [0; 1; 2] (List.init 3 ~f:ident))
  ; make "empty" (fun () ->
    is_true (List.is_empty []) <&&> is_false (List.is_empty [1]))
  ; make "last" (fun () ->
    expect (List.last [1;2;3;4]) ~is:(Some 4))
  ; make "mem" (fun () ->
    is_true (List.mem ~equal:len_equal ["x"; "xx"] "y"))
  ; make "mem" (fun () ->
    is_false (List.mem ~equal:len_equal ["x"; "xx"] "xxx"))
  ; make "dedupe" (fun () ->
    expect (List.dedupe ~equal:(=) [3;1;2;1;3]) ~is:[3;1;2])
  ; make "closure" (fun () ->
    expect (List.closure [1] ~equal:(=) ~f:(fun x ->
      if x < 10 then [x; x+1] else []))
    ~is:[1;2;3;4;5;6;7;8;9;10])
  ]
;;

let path_tests =
  let (^/) = Path.(^/) in
  let _expect_path p ~is:expected_p =
    expect ~eq:Path.equal ~to_string:Path.to_string p ~is:expected_p
  in
  let expect_dir d ~is:expected_d =
    expect ~eq:Path.Dir.equal ~to_string:Path.Dir.to_string d ~is:expected_d
  in
  let root_dir = Path.Abs.Dir.of_string "/" in
  [ make "/ of string" (fun () ->
    string_eq "/" (Path.to_string (Path.of_string "/")))
  ; make ". of string" (fun () ->
    string_eq "." (Path.to_string (Path.of_string ".")))
  ; make "current" (fun () ->
    let current = Path.Abs.Dir.current () in
    let path = Path.Dir.to_path current in
    let rel_path = Path.Abs.to_relative ~of_:current path in
    string_eq "." (Path.to_string rel_path))
  ; make "/../.." (fun () ->
    string_eq "/" (Path.to_string (Path.of_string "/../..")))
  ; make "../.." (fun () ->
    string_eq "../../" (Path.to_string (Path.of_string "../..")))
  ; make "/" (fun () ->
    let root = Path.Abs.of_string "/" in
    is_true (Path.is_directory root)
    <&&> expect (Path.file root) ~is:None
    <&&> expect_dir (Path.dir root) ~is:root_dir)
  ; make "/foo/bar" (fun () ->
    let dir = Path.Abs.Dir.of_string "/foo/" in
    let path = Path.Abs.of_string "/foo/bar" in
    is_false (Path.is_directory path)
    <&&> expect (Path.file path) ~is:(Some "bar")
    <&&> expect_dir (Path.dir path) ~is:dir)
  ; make "foo/bar" (fun () ->
    let dir = Path.Rel.Dir.of_string "foo" in
    let path = Path.Rel.of_string "foo/bar" in
    is_false (Path.is_directory path)
    <&&> expect (Path.file path) ~is:(Some "bar")
    <&&> string_eq "foo/" (Path.Dir.to_string (Path.dir path))
    <&&> expect_dir (Path.dir path) ~is:dir)
  ; make "ends in /" (fun () ->
    let path = Path.of_string "foo/" in
    is_true (Path.is_directory path))
  ; make "doesn't end in /" (fun () ->
    let path = Path.of_string "foo" in
    is_false (Path.is_directory path))
  ; make "normalize ../foo/.." (fun () ->
    let path = Path.Rel.of_string "../foo/.." in
    is_true (Path.is_directory path)
    <&&> expect (Path.file path) ~is:None
    <&&> expect_dir (Path.dir path) ~is:(Path.Rel.Dir.of_string ".."))
  ; make "normalize /../x" (fun () ->
    let path = Path.Abs.of_string "/../x" in
    is_false (Path.is_directory path)
    <&&> expect (Path.file path) ~is:(Some "x")
    <&&> expect_dir (Path.dir path) ~is:root_dir)
  (* TODO: similar checks for Path.Abs.Dir.of_string *)
  ; make "not rel" (fun () ->
    try
      ignore (Path.Abs.of_string "foo");
      Result.Fail "of_string succeeded"
    with _ -> Result.Pass)
  ; make "not abs" (fun () ->
    try
      ignore (Path.Rel.of_string "/foo");
      Result.Fail "of_string succeeded"
    with _ -> Result.Pass)
  ; make "concat abs" (fun () ->
    let path = root_dir ^/ "foo" in
    string_eq "/foo" (Path.to_string path)
    <&&> expect (Path.file path) ~is:(Some "foo")
    <&&> expect_dir (Path.dir path) ~is:root_dir)
  ; make "concat rel" (fun () ->
    let dir = Path.Rel.Dir.of_string "foo/bar" in
    let path = dir ^/ "../x" in
    expect (Path.file path) ~is:(Some "x")
    <&&> expect_dir (Path.dir path) ~is:(Path.Rel.Dir.of_string "foo"))
  ; make "concat abs" (fun () ->
    let dir = Path.Abs.Dir.of_string "/foo/bar" in
    let path = dir ^/ "../../root/x" in
    expect (Path.file path) ~is:(Some "x")
    <&&> expect_dir (Path.dir path) ~is:(Path.Abs.Dir.of_string "/root"))
  ; make "to-rel" (fun () ->
    let dir = Path.Abs.Dir.of_string "/home/datkin" in
    let path = Path.Abs.of_string "/home/rob/x" in
    let rel_path = Path.Abs.to_relative ~of_:dir path in
    is_false (Path.is_directory rel_path)
    <&&> expect (Path.file rel_path) ~is:(Some "x")
    <&&> expect_dir (Path.dir rel_path) ~is:(Path.Rel.Dir.of_string "../rob")
    <&&> string_eq "../rob/x" (Path.to_string rel_path))
  ; make "to-abs" (fun () ->
    let dir = Path.Abs.Dir.of_string "/home/datkin" in
    let path = Path.Rel.of_string "../rob/x" in
    let abs_path = Path.Rel.to_absolute ~of_:dir path in
    is_false (Path.is_directory abs_path)
    <&&> expect (Path.file abs_path) ~is:(Some "x")
    <&&> expect (Path.dir abs_path) ~is:(Path.Abs.Dir.of_string "/home/rob")
    <&&> string_eq "/home/rob/x" (Path.to_string abs_path))
  ]
;;

let tests =
  string_tests
  @ list_tests
  @ path_tests
;;

let () = run_all_and_report tests ;;
