(* Directed graph. *)
module type S = sig
  type node
  type edge = { from: node; to_: node }
  type t

  (*
  module Set : Set.S with type elt = node
  *)

  val equal: t -> t -> bool

  val empty: t

  val nodes: t -> node list
  val edges: t -> edge list

  val has_node: t -> node -> bool
  val has_edge: t -> from:node -> to_:node -> bool

  val add_node: t -> node -> t
  val add_edge: t -> from:node -> to_:node -> t

  val follow: t -> node -> node list option
  val rewind: t -> node -> node list option

  val filter_nodes: t -> f:(node -> bool) -> t
  val filter_edges: t -> f:(edge -> bool) -> t

  val map: t -> f:(edge -> edge list) -> t

  val topological_order: t -> node list option

  val of_edges: edge list -> t
  val of_list: (node * node) list -> t

  val to_list: t -> (node * node) list

  val dump
    : t
    -> (node -> string)
    -> (string * [ `In of string list ] * [ `Out of string list ]) list
end

module Make(Node : sig
  type t
  val compare: t -> t -> int
end) : S with type node = Node.t
