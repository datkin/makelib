type t

val current: unit -> t

val of_abs: string -> t
val to_abs: t -> string

val of_rel: ?in_:t -> string -> t
val to_rel: ?in_:t -> t -> t

val (^/): t -> string -> t

module Map : Map.S with type key := t
