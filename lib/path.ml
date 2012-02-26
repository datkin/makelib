open Util

(* Absolute paths only! *)
type t = string;;

exception Non_absolute_path of string;;
exception Non_relative_path of string;;

let is_absolute path = String.is_prefix ~prefix:"/" path;;
let is_relative path = Filename.is_relative path;;

(* Maybe this should just be the internal representation? *)
let segments t =
  match String.split t ~on:'/' with
  | "" :: segments -> segments
  | _ -> raise (Non_absolute_path t)
;;

let of_abs path =
  if is_absolute path then
    (path: t)
  else
    raise (Non_absolute_path path)
;;

let to_abs t = (t: string);;

let current () = of_abs (Unix.getcwd ()) ;;

(* Evaluate a path with '.' and '..' *)
let eval segments =
  let rec eval rev_segments segments =
    match rev_segments with
    | []
    | ".." :: [] -> segments
    | ".." :: _ :: reg_segments -> eval rev_segments segments
    | "." :: rev_segments -> eval rev_segments segments
    | segment ::      rev_segents -> eval rev_segments (segment :: segments)
  in
  let segments = eval (List.rev segments) [] in
  ("/" ^ String.concat segments ~sep:"/")
;;

(* Fully evaluate a relative path. *)
let of_rel ?in_:root path =
  if is_relative path then begin
    let root =
      match root with
      | Some root -> root
      | None -> current ()
    in
    let root_segments = String.split root ~on:'/' in
    let path_segments = String.split path ~on:'/' in
    eval (root_segments @ path_segments)
  end else
    raise (Non_relative_path path)
;;

(*
path to: /home/datkin/foo/bar
in: /home/datkin/x/
is: ../foo/bar

/home/datkin/x
relative to
/home/datkin/foo/bar/
is
../../x

strip the common prefix, then add a double dot for each bit "to" is longer than
"in"
*)
let rec drop_shared_prefix a_segments b_segments =
  match a_segments, b_segments with
  | x :: a_segments
  , y :: b_segments when x = y -> drop_shared_prefix a_segments b_segments
  | a_segments, b_segments -> a_segments, b_segments
;;

let to_rel ?in_:root t =
  let root =
    match root with
    | Some root -> root
    | None -> current ()
  in
  let root_suffix, t_suffix = drop_shared_prefix (segments root) (segments t) in
  (* How much longer is the t-suffix than the root suffix? *)
  let double_dots = abs ((List.length root_suffix) - (List.length t_suffix)) in
  let double_dots = List.init double_dots ~f:(const "..") in
  String.concat (double_dots @ t_suffix) ~sep:"/"
;;

let (^/) root path =
  of_rel path ~in_:root
;;

module Map = Map.Make(String);;
