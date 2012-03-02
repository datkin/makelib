open Util

module type S = sig
  type node
  type edge = { from: node; to_: node }
  type t

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

  (* ??? *)
  val map: t -> f:(edge -> edge list) -> t

  val topological_order: t -> node list

  val of_list: edge list -> t

  val dump
    : t
    -> (node -> string)
    -> (string * [ `In of string list ] * [ `Out of string list ]) list
end

module Make (Node : sig
  type t
  val compare: t -> t -> int
end) = struct
  type node = Node.t
  type edge = { from: node; to_: node }

  module Node_set = Set.Make(Node)

  module Edge_info = struct
    type t =
      { in_: Node_set.t
      ; out: Node_set.t }

    let empty =
      { in_ = Node_set.empty
      ; out = Node_set.empty }

    let equal t1 t2 =
      Node_set.equal t1.in_ t2.in_
      && Node_set.equal t1.out t2.out

    let cons x xs = x :: xs

    let out t = Node_set.fold cons t.out []

    let in_ t = Node_set.fold cons t.in_ []

    let has_in t node =
      Node_set.mem node t.in_

    let has_out t node =
      Node_set.mem node t.out

    let add_in t node =
      { t with in_ = Node_set.add node t.in_ }

    let add_out t node =
      { t with out = Node_set.add node t.out }

    let filter t ~f:keep =
      { in_ = Node_set.filter keep t.in_
      ; out = Node_set.filter keep t.out }

      (*
    let remove_in t node =
      { t with in_ = Node_set.remove node t.in_ }

    let remove_out t node =
      { t with out = Node_set.remove node t.out }
*)
  end

  module Map = Map.Make(Node)

  type t = Edge_info.t Map.t

  let equal t1 t2 =
    Map.equal Edge_info.equal t1 t2

  let empty = Map.empty

  let nodes t =
    List.map (Map.bindings t) ~f:(fun (node, _edge_info) -> node)

  let edges t =
    let collect_edges src edge_info edges =
      let new_edges =
        List.map (Edge_info.out edge_info) ~f:(fun dest -> { from=src; to_=dest })
      in
      new_edges @ edges
    in
    Map.fold collect_edges t []

  let has_node t node =
    Map.mem node t

  let has_edge t ~from:src ~to_:dest =
    (* Check invariant: dest has_in src? *)
    try
      let edge_info = Map.find src t in
      Edge_info.has_out edge_info dest
    with Not_found -> false

  let add_node t node =
    if Map.mem node t then
      Map.add node Edge_info.empty t
    else
      t

  let add_edge t ~from:src ~to_:dest =
    let add node ~to_:target ~with_:add t =
      let info =
        try Map.find target t
        with Not_found -> Edge_info.empty
      in
      let info = add info node in
      Map.add target info t
    in
    let t = add dest ~to_:src ~with_:Edge_info.add_out t in
    let t = add src ~to_:dest ~with_:Edge_info.add_in t in
    t

    (*
    let src_info =
      try Map.find src t
      with Not_found -> Edge_info.empty
    in
    let dest_info =
      try Map.find dest t
      with Not_found -> Edge_info.empty
    in
    let src_info = Edge_info.add_out src_info dest in
    let dest_info = Edge_info.add_in dest_info src in
    let map = Map.add src (Node_set.add dest_info nodes) t in
  *)

  let follow t node =
    try
      let info = Map.find node t in
      Some (Edge_info.out info)
    with Not_found ->
      None

  let rewind t node =
    try
      let info = Map.find node t in
      Some (Edge_info.in_ info)
    with Not_found ->
      None

  let filter_edges t ~f:keep =
    let filter_edges src edge_info t =
      List.fold (Edge_info.out edge_info) ~init:t ~f:(fun t dest ->
        if keep { from = src; to_ = dest } then
          add_edge t ~from:src ~to_:dest
        else t)
    in
    Map.fold filter_edges t Map.empty

  let filter_nodes t ~f:keep =
    let nodes = List.filter (nodes t) ~f:keep in
    let keep = List.mem ~set:nodes in
    let filter_nodes node edge_info t =
      if keep node then
        let edge_info = Edge_info.filter edge_info ~f:keep in
        Map.add node edge_info t
      else t
    in
    Map.fold filter_nodes t Map.empty

  let of_list edges =
    List.fold edges ~init:Map.empty ~f:(fun t { from=src; to_=dest } ->
      add_edge t ~from:src ~to_:dest)

  let map t ~f =
    of_list (List.flatten (List.map (edges t) ~f))

  let topological_order _t = []

  let dump t node_to_string =
    List.map (Map.bindings t) ~f:(fun (node, edge_info) ->
      (node_to_string node)
      , `In (List.map ~f:node_to_string (Edge_info.in_ edge_info))
      , `Out (List.map ~f:node_to_string (Edge_info.out edge_info)))

    (*
    let remove node t =
      let edge_info =
        try Map.find node t
        with Not_found -> Edge_info.empty
      in
      let srcs = Edge_info.in_ edge_info in
      let dests = Edge_info.out edge_info in
      let t = Map.remove node t in
      let t = Map.add 
      *)
end
