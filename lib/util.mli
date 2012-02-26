module String : sig
  type t = string

  val concat: sep:t -> t list -> t

  val length: t -> int

  val is_prefix: t -> prefix:t -> bool

  val is_suffix: t -> suffix:t -> bool

  val split: t -> on:char -> t list

  val lsplit2: t -> on:char -> (t * t) option

  val rsplit2: t -> on:char -> (t * t) option

  val compare: t -> t -> int

  val is_empty: t -> bool
end

module List : sig
  type 'a t = 'a list

  include module type of ListLabels

  val init: int -> f:(int -> 'a) -> 'a t
end

module Unix : sig
  include module type of UnixLabels
end

val failwithf: ('a, unit, string, unit -> 'b) format4 -> 'a

val const: 'a -> 'b -> 'a
