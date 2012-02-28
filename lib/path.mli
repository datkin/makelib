module T : sig
  type abs
  type rel
  type either

  type 'a t
end

open T

module Abs : sig
  type t = abs T.t

  val of_string: string -> t
  val to_string: t -> string

  val current: unit -> t

  val to_relative: ?of_:t -> t -> rel T.t
end

module Rel : sig
  type t = rel T.t

  val of_string: string -> t
  val to_string: t -> string

  val to_absolute: ?of_:abs T.t -> t -> abs T.t

  module Map : Map.S with type key := t
end

(* None for directories. *)
val basename: 'a t -> string option

(* [ t ^/ path] = [abs_of_rel ~in_:t (of_rel path)] *)
val (^/): abs t -> string -> abs t

val of_string: string -> either t

val to_string: 'a t -> string
