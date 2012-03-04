module T : sig
  type abs
  type rel
  type either

  type 'a t
end

type 'a t = 'a T.t
open T

module Dir : sig
  type 'a t
  val of_string: string -> either t
  val to_string: 'a t -> string

  val to_path: 'a t -> 'a T.t

  val equal: 'a t -> 'a t -> bool
end

module Abs : sig
  type t = abs T.t

  val of_string: string -> t
  val to_string: t -> string

  module Dir : sig
    type t = abs Dir.t
    val of_string: string -> t

    val current: unit -> t
  end

  val to_relative: ?of_:Dir.t -> t -> rel T.t

  module Map : Map.S with type key = t
end

module Rel : sig
  type t = rel T.t

  val of_string: string -> t
  val to_string: t -> string

  val to_absolute: ?of_:Abs.Dir.t -> t -> Abs.t

  module Dir : sig
    type t = rel Dir.t
    val of_string: string -> t
  end

  module Map : Map.S with type key = t
end

(* None for directories. *)
val file: 'a t -> string option

val dir: 'a t -> 'a Dir.t

val is_directory: 'a t -> bool

val split: 'a t -> 'a Dir.t * string option

val basename: 'a t -> string

val equal: 'a t -> 'a t -> bool

val compare: 'a t -> 'a t -> int

(* [ t ^/ path] = [Rel.to_absolute ?of:t (Rel.of_string rel)] *)
val (^/): 'a Dir.t -> string -> 'a t

val of_string: string -> either t

val to_string: 'a t -> string

val pp: Format.formatter -> 'a t -> unit

module Map : Map.S with type key = either T.t
