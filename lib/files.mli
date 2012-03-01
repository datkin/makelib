(* Define common Filesystem operations on paths and dirs. *)

val in_directory: 'a Dir.t -> 'a Path.t list

val exists: 'a Path.t -> bool

val stat: 'a Path.t -> Unix.stats

val readlink: 'a Path.t -> 'a Path.t

(* More operations like: exists, stat, follow_link *)
