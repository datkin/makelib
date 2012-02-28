open Util

(* TODO: ALL OF THIS WILL BLOW UP IF THERE ARE /s IN YOUR FILE NAMES. *)

module T = struct
  type abs = unit
  type rel = unit
  type either = unit

  type 'a t =
    { dir: string list
    ; basename: string option
    ; kind: [ `Absolute | `Relative ] }
end

include T

exception Non_absolute_path of rel t
exception Non_relative_path of abs t
exception Empty_path of string

let basename t = t.basename;;

(* We assume dir has already been processed to determine if it's a relative or
 * absolute path. Therefore, it's safe to remove any "" segments. Treat "x/.."
 * as a redex. If it's an absolute path, all leading ".."s can be dropped. *)
let normalize kind dir =
  let rec eval dir acc =
    match dir, acc with
    | ".." :: dir, ".." :: acc ->
      if kind = `Absolute then
        eval dir acc (* should never happend *)
      else
        eval dir (".." :: ".." :: acc)
    | ".." :: dir, _ :: acc ->
      eval dir acc
    | ".." :: dir, [] ->
      if kind = `Absolute then
        eval dir acc
      else
        eval dir (".." :: acc)
    | segment :: dir, acc -> eval dir (segment :: acc)
    | [], _ -> acc
  in
  let dir = List.filter dir ~f:((<>) ".") in
  let dir = List.filter dir ~f:((<>) "") in
  List.rev (eval dir [])
;;

let of_string path: either t =
  (* Ensure '.', '..', '/..', and '/.' are treated as directories, not
   * filenames by adding a '/' on the end. *)
  let path =
    if List.exists ["/."; "/.."] ~f:(fun suffix -> String.is_suffix path ~suffix)
       || List.mem path ~set:["."; ".."]
    then path ^ "/"
    else path
  in
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

module Abs = struct
  type t = abs T.t

  (* Enforce the constraint that only `Absolute paths can have type Abs.t *)
  let of_string path: t =
    let t = of_string path in
    match t.kind with
    | `Absolute -> {t with kind = `Absolute}
    | `Relative -> raise (Non_absolute_path {t with kind = `Relative})

  let to_string t =
    match t.dir, t.basename with
    | [], None -> "/"
    | [], Some basename -> "/" ^ basename
    | dir, basename ->
      let dir = "/" ^ String.concat dir ~sep:"/" ^ "/" in
      match basename with
      | Some basename -> dir ^ basename
      | None -> dir

  let current () =
    of_string (Unix.getcwd () ^ "/")

  let current_unless path_opt =
    match path_opt with
    | Some path -> path
    | None -> current ()

  let rec drop_shared_prefix a_segments b_segments =
    match a_segments, b_segments with
    | x :: a_segments
    , y :: b_segments when x = y -> drop_shared_prefix a_segments b_segments
    | a_segments, b_segments -> a_segments, b_segments

  (* Examples:
   * root                  | target               | rel
   * ----------------------+----------------------+-----------------
   * /home/datkin/         | /home/datkin/x       | x
   * /home/foo/            | /home/datkin/x       | ../datkin/x
   * /home/foo             | /home/datkin/x       | datkin/x
   * /home/datkin/x/       | /home/datkin         | ../
   * /home/datkin/foo/bar/ | /home/datkin/x       | ../../x
   * /home/datkin/x/       | /home/datkin/foo/bar | ../foo/bar
   * /home/datkin/x/       | /root                | ../../root
   * /root/                | /home/datkin/x       | ../home/datkin/x
   *)
  let to_relative ?of_:root t =
    let root = current_unless root in
    let root_suffix, t_suffix = drop_shared_prefix root.dir t.dir in
    let double_dots = List.init (List.length root_suffix) ~f:(const "..") in
    { dir = double_dots @ t_suffix; basename = t.basename; kind = `Relative }

  module Map = Map.Make(struct
    type t = abs T.t
    let compare (a: t) b = compare a b
  end)
end

module Rel = struct
  type t = rel T.t

  (* Enforce the constraint that only `Relative paths can have type Abs.t *)
  let of_string path =
    let t = of_string path in
    match t.kind with
    | `Relative -> {t with kind = `Relative}
    | `Absolute -> raise (Non_relative_path {t with kind = `Absolute})

  let to_string t =
    match t.dir, t.basename with
    | [], None -> "."
    | [], Some basename -> basename
    | dir, basename ->
      let dir = String.concat dir ~sep:"/" ^ "/" in
      match basename with
      | Some basename -> dir ^ basename
      | None -> dir

  let to_absolute ?of_:root t =
    let root = Abs.current_unless root in
    let kind = `Absolute in
    let dir = normalize kind (root.dir @ t.dir) in
    { dir = dir; basename = t.basename; kind = kind }

  module Map = Map.Make(struct
    type t = rel T.t
    let compare (a: t) b = compare a b
  end)
end

let (^/) root path =
  Rel.to_absolute ~of_:root (Rel.of_string path)
;;

let to_string t =
  match t.kind with
  | `Absolute -> Abs.to_string t
  | `Relative -> Rel.to_string t
;;
