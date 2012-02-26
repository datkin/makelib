type abs
type rel

type 'a t = private
  { dir: string list
  ; basename: string option (* None for directories. *)
  ; kind: 'a }

val current: unit -> abs t

val basename: 'a t -> string option

(* Paths ending with a / are assumed to be directories. *)
val of_abs: string -> abs t
val to_abs: abs t -> string

val of_rel: string -> rel t
val to_rel: rel t -> string

val abs_of_rel: ?in_:abs t -> rel t -> abs t
val rel_of_abs: ?in_:abs t -> abs t -> rel t

(* [ t ^/ path] = [abs_of_rel ~in_:t (of_rel path)] *)
val (^/): abs t -> string -> abs t

module Make_map(Kind: sig type t end): Map.S with type key = Kind.t t
