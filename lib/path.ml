open Util

(* TODO: ALL OF THIS WILL BLOW UP IF THERE ARE /s IN YOUR PATH. *)

(* Phantom types *)
type abs = [ `Absolute ];;
type rel = [ `Relative ];;
(* type either = [ #abs | #rel ];; *)
type either = [ `Absolute | `Relative ];;

type 'a t =
  { dir: string list
  ; basename: string option
  ; kind: 'a }
;;

exception Non_absolute_path of rel t;;
exception Non_relative_path of abs t;;
exception Empty_path of string;;

let basename t = t.basename;;

(* We assume dir has already been processed to determine if it's a relative or
 * absolute path. Therefore, it's safe to remove any "" segments. If it's an
 * absolute path, all ".." should be removed. If it's a relative path, we can
 * remove them in the middle. *)
let normalize kind dir =
  let rec eval dir acc =
    match dir with
    | ".." :: ".." :: dir ->
      if kind = `Absolute then
        eval dir acc
      else
        eval (".." :: dir) (".." :: acc)
    | _ :: ".." :: dir ->
      eval dir acc
    | ".." :: dir ->
      if kind = `Absolute then
        eval dir acc
      else
        eval dir (".." :: acc)
        (* consider: ../../../foo -> .. ../../foo -> ../.. ../foo ->  *)
    | segment :: dir -> eval dir (segment :: acc)
    | [] -> acc
  in
  let dir = List.filter dir ~f:((=) ".") in
  let dir = List.filter dir ~f:((=) "") in
  List.rev (eval dir [])
;;

let of_string path: either t =
  match String.rsplit2 path ~on:'/' with
  | Some (dir, basename) ->
    let basename =
      match basename with
      | "" -> None
      | x -> Some x
    in
    let kind, dir =
      match String.split dir ~on:'/' with
      | "" :: dir -> `Absolute, dir
      | dir -> `Relative, dir
    in
    let dir = normalize kind dir in
    { dir; basename; kind }
  | None ->
    (* The path has no slash, so it must be relative. *)
    if String.is_empty path then
      raise (Empty_path path)
    else
      { dir = []; basename = Some path; kind = `Relative }
;;

let of_abs path: abs t =
  let t = of_string path in
  match t.kind with
  | `Absolute -> {t with kind = `Absolute}
  | `Relative -> raise (Non_absolute_path {t with kind = `Relative})
;;

let of_rel path: rel t =
  let t = of_string path in
  match t.kind with
  | `Relative -> {t with kind = `Relative}
  | `Absolute -> raise (Non_relative_path {t with kind = `Absolute})
;;

let to_abs t =
  match t.dir, t.basename with
  | [], None -> "/"
  | dir, basename ->
    let dir = "/" ^ String.concat dir ~sep:"/" ^ "/" in
    match basename with
    | Some basename -> dir ^ basename
    | None -> dir
;;

let to_rel t =
  match t.dir, t.basename with
  | [], None -> "."
  | dir, basename ->
    let dir = String.concat dir ~sep:"/" ^ "/" in
    match basename with
    | Some basename -> dir ^ basename
    | None -> dir
;;

let current () =
  of_abs (Unix.getcwd () ^ "/")
;;

let current_unless path_opt =
  match path_opt with
  | Some path -> path
  | None -> current ()
;;

let abs_of_rel ?in_:root path =
  let root = current_unless root in
  let kind = `Absolute in
  let dir = normalize kind (root.dir @ path.dir) in
  { dir = dir; basename = path.basename; kind = kind }
;;

let rec drop_shared_prefix a_segments b_segments =
  match a_segments, b_segments with
  | x :: a_segments
  , y :: b_segments when x = y -> drop_shared_prefix a_segments b_segments
  | a_segments, b_segments -> a_segments, b_segments
;;

(* Examples:
 * root                 | target               | rel
 * ---------------------+----------------------+-------
 * /home/datkin         | /home/datkin/x       | x
 * /home/datkin/x       | /home/datkin         | ../
 * /home/datkin/foo/bar | /home/datkin/x       | ../../x
 * /home/datkin/x       | /home/datkin/foo/bar | ../foo/bar
 * /home/datkin/x       | /root                | ../../root
 * /root                | /home/datkin/x       | ../home/datkin/x
 *
 * Hmm, question: do we assume the "in" is always a directory?
 *)
let rel_of_abs ?in_:root t =
  let root = current_unless root in
  let root_suffix, t_suffix = drop_shared_prefix root.dir t.dir in
  let double_dots = List.init (List.length root_suffix) ~f:(const "..") in
  { dir = double_dots @ t_suffix; basename = t.basename; kind = `Relative }
;;

let (^/) root path =
  abs_of_rel ~in_:root (of_rel path)
;;

(*
module Map = Map.Make(String);;
*)
