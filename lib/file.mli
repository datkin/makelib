(* Define common Filesystem operations on paths and dirs. *)

val list: 'a Path.Dir.t -> 'a Path.t list

val exists: 'a Path.t -> bool

val stat: 'a Path.t -> Unix.stats

val readlink: 'a Path.t -> Path.T.either Path.t
