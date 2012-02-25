type t

val abs: string -> t

val (^/): t -> string -> t

module Map : Map.S with type key := t
