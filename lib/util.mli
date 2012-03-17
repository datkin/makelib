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

  val strip_ws: t -> t

  val pp: Format.formatter -> t -> unit
end

module List : sig
  type 'a t = 'a list

  include module type of ListLabels

  val init: int -> f:(int -> 'a) -> 'a t

  val is_empty: 'a t -> bool

  val last: 'a t -> 'a option

  val map: 'a t -> f:('a -> 'b) -> 'b t

  val mapi: 'a t -> f:(int -> 'a -> 'b) -> 'b t

  val fold: 'a t -> init:'b -> f:('b -> 'a -> 'b) -> 'b

  val mem: 'a list -> equal:('a -> 'a -> bool) -> 'a -> bool

  val dedupe: 'a list -> equal:('a -> 'a -> bool) -> 'a list

  val closure: 'a t -> equal:('a -> 'a -> bool) -> f:('a -> 'a list) -> 'a list

  val to_string: 'a t -> to_string:('a -> string) -> string

  val equal: 'a t -> 'a t -> equal:('a -> 'a -> bool) -> bool

  val filter_map: 'a t -> f:('a -> 'b option) -> 'b list

  val partition_map
    :  'a t
    -> f:('a -> [ `Fst of 'b | `Snd of 'c ])
    -> 'b list * 'c list

  val find_map: 'a t -> f:('a -> 'b option) -> 'b option

  val divide: 'a list -> at:int -> 'a list * 'a list
end

module Non_empty_list : sig
  type 'a t

  val create: 'a -> 'a t

  val add: 'a t -> 'a -> 'a t

  val to_list: 'a t -> 'a list

  val hd: 'a t -> 'a

  val split: 'a t -> 'a * 'a list

  val of_split: 'a -> 'a list -> 'a t
end

module Map : sig
  module type S = sig
    type key

    type 'a t

    val empty: 'a t

    val is_empty: 'a t -> bool

    val mem: 'a t -> key -> bool

    val add: 'a t -> key -> 'a -> 'a t

    val find: 'a t -> key -> 'a option

    val remove: 'a t -> key -> 'a t

    val compare: 'a t -> 'a t -> cmp:('a -> 'a -> int) -> int

    val equal: 'a t -> 'a t -> eq:('a -> 'a -> bool) -> bool

    val fold: 'a t -> init:'b -> f:('b -> key:key -> data:'a -> 'b) -> 'b

    val map: 'a t -> f:('a -> 'b) -> 'b t

    val mapi: 'a t -> f:(key -> 'a -> 'b) -> 'b t

    val filter_map: 'a t -> f:('a -> 'b option) -> 'b t

    val keys: 'a t -> key list

    val data: 'a t -> 'a list

    val to_alist: 'a t -> (key * 'a) list
  end

  module Make(Ord: Map.OrderedType): S with type key = Ord.t
end

module Unix : sig
  include module type of UnixLabels
end

val sprintf: ('a, unit, string) format -> 'a
val failwithf: ('a, unit, string, unit -> 'b) format4 -> 'a

val const: 'a -> 'b -> 'a

val ident: 'a -> 'a
