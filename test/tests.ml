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

let list_tests =
  let open Util in
  [ make "list string" (fun () ->
    string_eq "[a; b]" (List.to_string ["a"; "b"] ~to_string:ident))
  ; make "init" (fun () ->
    list_eq ~to_string:string_of_int [0; 1; 2] (List.init 3 ~f:ident))
  ]
;;

let tests =
  string_tests
  @ list_tests
;;

let () = run_all_and_report tests ;;
