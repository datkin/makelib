open Util

(* TODO: ALL OF THIS WILL BLOW UP IF THERE ARE /s IN YOUR FILE NAMES. *)

module T = struct
  type abs = unit
  type rel = unit
  type either = unit

  type 'a t =
    { dir: string list
    ; file: string option
    ; kind: [ `Absolute | `Relative ] }
end

include T

exception Non_absolute_path of rel t
exception Non_relative_path of abs t
exception Empty_path of string

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
       || List.mem ["."; ".."] ~equal:(=) path
    then path ^ "/"
    else path
  in
  match String.rsplit2 path ~on:'/' with
  | Some (dir, file) ->
    let file =
      match file with
      | "" -> None
      | x -> Some x
    in
    let kind, dir =
      match String.split dir ~on:'/' with
      | "" :: dir -> `Absolute, dir
      | dir -> `Relative, dir
    in
    let dir = normalize kind dir in
    { dir; file; kind }
  | None ->
    (* The path has no slash, so it must be relative. *)
    if String.is_empty path then
      raise (Empty_path path)
    else
      { dir = []; file = Some path; kind = `Relative }
;;

module Dir = struct
  type 'a t =
    { dir: string list
    ; kind: [ `Absolute | `Relative ] }

  let of_string path: either t =
    let kind, segments =
      match String.split path ~on:'/' with
      | "" :: segments -> `Absolute, segments
      | segments -> `Relative, segments
    in
    let dir = normalize kind segments in
    { dir = dir
    ; kind = kind }

  let to_path t =
    { T.dir = t.dir; file = None; kind = t.kind }

  let to_string t =
    match t.kind, t.dir with
    | `Relative, [] -> "."
    | `Relative, segments -> String.concat segments ~sep:"/" ^ "/"
    | `Absolute, [] -> "/"
    | `Absolute, segments -> "/" ^ String.concat segments ~sep:"/" ^ "/"

  let equal t1 t2 = compare t1 t2 = 0

  exception Non_absolute_dir of rel t
  exception Non_relative_dir of abs t
end

module Abs = struct
  type t = abs T.t

  (* Enforce the constraint that only `Absolute paths can have type Abs.t *)
  let of_string path: t =
    let t = of_string path in
    match t.kind with
    | `Absolute -> {t with kind = `Absolute}
    | `Relative -> raise (Non_absolute_path {t with kind = `Relative})

  module Dir = struct
    type t = abs Dir.t

    let dir t = t.Dir.dir

    let of_string path =
      let t = Dir.of_string path in
      match t.Dir.kind with
      | `Absolute -> {t with Dir.kind = `Absolute}
      | `Relative -> raise (Dir.Non_absolute_dir {t with Dir.kind = `Relative})

    let current () =
      of_string (Unix.getcwd () ^ "/")
  end

  let to_string t =
    match t.dir, t.file with
    | [], None -> "/"
    | [], Some file -> "/" ^ file
    | dir, file ->
      let dir = "/" ^ String.concat dir ~sep:"/" ^ "/" in
      match file with
      | Some file -> dir ^ file
      | None -> dir

  let current_unless path_opt =
    match path_opt with
    | Some path -> path
    | None -> Dir.current ()

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
    let root_suffix, t_suffix = drop_shared_prefix (Dir.dir root) t.dir in
    let double_dots = List.init (List.length root_suffix) ~f:(const "..") in
    { dir = double_dots @ t_suffix; file = t.file; kind = `Relative }

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

  module Dir = struct
    type t = rel Dir.t

    let dir t = t.Dir.dir

    let of_string path =
      let t = Dir.of_string path in
      match t.Dir.kind with
      | `Relative -> {t with Dir.kind = `Relative}
      | `Absolute -> raise (Dir.Non_relative_dir {t with Dir.kind = `Absolute})

    let current = of_string "."
    let up = of_string ".."
  end

  let to_string t =
    match t.dir, t.file with
    | [], None -> "."
    | [], Some file -> file
    | dir, file ->
      let dir = String.concat dir ~sep:"/" ^ "/" in
      match file with
      | Some file -> dir ^ file
      | None -> dir

  let to_absolute ?of_:root t =
    let root = Abs.current_unless root in
    let kind = `Absolute in
    let dir = normalize kind ((Dir.dir root) @ t.dir) in
    { dir = dir; file = t.file; kind = kind }

  module Map = Map.Make(struct
    type t = rel T.t
    let compare (a: t) b = compare a b
  end)
end

let compare (a: 'a t) b = compare a b;;

let equal (a: 'a t) b = compare a b = 0;;

let split t =
  { Dir.dir = t.dir; kind = t.kind }, t.file
;;

let is_directory t =
  match t.file with
  | Some _ -> false
  | None -> true
;;

let file t = t.file ;;

let dir t =
  { Dir.dir = t.dir; kind = t.kind }
;;

let (^/) root path =
  (* Ensure path is relative... *)
  let path = Rel.to_string (Rel.of_string path) in
  let t = of_string (Dir.to_string root ^ "/" ^ path) in
  { t with kind = root.Dir.kind }
;;

let to_string t =
  match t.kind with
  | `Absolute -> Abs.to_string t
  | `Relative -> Rel.to_string t
;;

let basename path =
  Filename.basename (to_string path)
;;

let pp formatter t =
  String.pp formatter (to_string t)

module Map = Map.Make(struct
  type t = either T.t
  let compare (a: t) b = compare a b
end)
