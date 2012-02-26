(* CR datkin: rename this path? File suggests the file exists? Or contains file info. *)
type t

val of_path: string -> t

(* If to_ is None, use cwd
val of_relative_path: ?to_:File.t -> string -> t
*)

val (^/): t -> string -> t

val absolute_path: t -> string
(*
val relative_path: ?to_:File.t -> t -> string
*)

module Map : Map.S with type key := t
