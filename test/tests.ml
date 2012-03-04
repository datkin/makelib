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

let graph_tests =
  let open Util in
  let module G = Graph.Make(struct
    type t = int
    let compare = compare
  end) in
  [ make "empty" (fun () ->
    list_eq [] (G.nodes G.empty)
    <&&> list_eq [] (G.edges G.empty)
    <&&> expect (G.follow G.empty 1) ~is:None
    <&&> expect (G.rewind G.empty 1) ~is:None
    (* TODO: Some []? *)
    <&&> expect (G.topological_order G.empty) ~is:None)
  ; make "has node" (fun () ->
    let g1 = G.add_node G.empty 1 in
    is_false (G.has_node G.empty 1)
    <&&> is_true (G.has_node g1 1)
    <&&> is_false (G.has_node g1 2))
  ; make "singleton" (fun () ->
    let g = G.add_node G.empty 1 in
    list_eq [1] (G.nodes g)
    <&&> list_eq [] (G.edges g)
    <&&> expect (G.topological_order g) ~is:(Some [1])
    <&&> expect (G.follow g 1) ~is:(Some [])
    <&&> expect (G.rewind g 1) ~is:(Some []))
  ; make "a -> b" (fun () ->
    let g1 =
      let g = G.empty in
      let g = G.add_node g 1 in
      let g = G.add_node g 2 in
      let g = G.add_edge g ~from:1 ~to_:2 in
      g
    in
    let g2 =
      let g = G.empty in
      let g = G.add_edge g ~from:1 ~to_:2 in
      g
    in
    let g3 = G.of_edges [{G.from = 1; to_ = 2}] in
    let g4 = G.of_list [1,2] in
    let gs = [g1; g2; g3; g4] in
    is_true (G.equal g1 g2) <&&> is_true (G.equal g2 g1)
    <&&> is_true (G.equal g1 g3) <&&> is_true (G.equal g2 g3)
    <&&> is_true (G.equal g1 g4) <&&> is_true (G.equal g2 g4)
    <&&> is_true (G.equal g3 g4) <&&> is_true (G.equal g4 g4)
    <&&> is_true (List.for_all gs ~f:(fun g ->
      List.equal ~equal:(=) [1;2] (G.nodes g)))
    <&&> is_true (List.for_all gs ~f:(fun g ->
      List.equal ~equal:(=) [{G.from = 1; to_ = 2}] (G.edges g)))
    <&&> is_true (List.for_all gs ~f:(fun g ->
      (G.topological_order g) = (Some [1;2]))))
  ; make "reverse" (fun () ->
    let g = G.of_list [1, 2; 2, 3] in
    let g' =
      G.map g ~f:(fun {G.from = src; to_ = dest} ->
        [{G.from = dest; to_= src}])
    in
    expect (G.topological_order g) ~is:(Some [1;2;3])
    <&&> expect (G.topological_order g') ~is:(Some [3;2;1]))
  ]
;;

let tests =
  string_tests
  @ list_tests
  @ path_tests
  @ graph_tests
;;

let () = run_all_and_report tests ;;
