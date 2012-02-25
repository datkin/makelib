(* TODO: Move this into a util module so the module name [String] isn't
 * ambiguous? *)

include StringLabels

type t = string

val is_prefix: t -> prefix:t -> bool

val is_suffix: t -> suffix:t -> bool

val split: t -> on:char -> t list

val lsplit2: t -> on:char -> (t * t) option

val rsplit2: t -> on:char -> (t * t) option
