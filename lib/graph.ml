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

module Make (Node : sig
  type t
  val compare: t -> t -> int
end) = struct
  type node = Node.t
  type edge = { from: node; to_: node }

  module Node_set = Set.Make(Node)

  let nodes_equal n1 n2 =
    compare n1 n2 = 0

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

    let connected t =
      let nodes = Node_set.union t.out t.in_ in
      Node_set.fold cons nodes []

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
  end

  (* CR datkin: Could probably change some implementations now that Map is
   * sweeksified. *)
  module Map = Map.Make(Node)

  type t = Edge_info.t Map.t

  let equal t1 t2 =
    Map.equal ~eq:Edge_info.equal t1 t2

  let empty = Map.empty

  let nodes t = Map.keys t

  let edges t =
    Map.fold t ~init:[] ~f:(fun edges ~key:src ~data:edge_info ->
      let new_edges =
        List.map (Edge_info.out edge_info) ~f:(fun dest -> { from=src; to_=dest })
      in
      new_edges @ edges)

  let has_node t node = Map.mem t node

  let has_edge t ~from:src ~to_:dest =
    (* Check invariant: dest has_in src? *)
    match Map.find t src with
    | Some edge_info -> Edge_info.has_out edge_info dest
    | None -> false

  let add_node t node =
    if has_node t node then t
    else Map.add t node Edge_info.empty

  let add_edge t ~from:src ~to_:dest =
    let add node ~to_:target ~with_:add t =
      let info =
        match Map.find t target with
        | Some edge_info -> edge_info
        | None -> Edge_info.empty
      in
      let info = add info node in
      Map.add t target info
    in
    let t = add dest ~to_:src ~with_:Edge_info.add_out t in
    let t = add src ~to_:dest ~with_:Edge_info.add_in t in
    t

  let follow t node =
    match Map.find t node with
    | Some edge_info -> Some (Edge_info.out edge_info)
    | None -> None

  let rewind t node =
    match Map.find t node with
    | Some edge_info -> Some (Edge_info.in_ edge_info)
    | None -> None

  let filter_edges t ~f:keep =
    let filter_edges t ~key:src ~data:edge_info =
      List.fold (Edge_info.out edge_info) ~init:t ~f:(fun t dest ->
        if keep { from = src; to_ = dest } then
          add_edge t ~from:src ~to_:dest
        else t)
    in
    let base_map = List.fold (nodes t) ~init:Map.empty ~f:add_node in
    Map.fold t ~init:base_map ~f:filter_edges

  let remove_node t node =
    let (<>) x y = not (nodes_equal x y) in
    match Map.find t node with
    | Some edge_info ->
      let neighbors = Edge_info.connected edge_info in
      let t =
        List.fold neighbors ~init:t ~f:(fun t neighbor ->
          match Map.find t neighbor with
          | Some edge_info ->
            let edge_info = Edge_info.filter edge_info ~f:((<>) node) in
            Map.add t neighbor edge_info
          | None -> t)
      in
      Map.remove t node
    (* Probably shouldn't happen! *)
    | None -> t

  let filter_nodes t ~f:keep =
    let nodes = List.filter (nodes t) ~f:keep in
    let keep = List.mem nodes ~equal:nodes_equal in
    (* TODO: This could be optimized b/c we know exactly which edge_infos need to
     * be updated. *)
    let filter_nodes t ~key:node ~data:edge_info =
      if keep node then
        let edge_info = Edge_info.filter edge_info ~f:keep in
        Map.add t node edge_info
      else t
    in
    Map.fold t ~init:Map.empty ~f:filter_nodes

  let of_edges edges =
    List.fold edges ~init:Map.empty ~f:(fun t { from=src; to_=dest } ->
      add_edge t ~from:src ~to_:dest)

  let of_list edge_tuples =
    of_edges (List.map edge_tuples ~f:(fun (src, dest) ->
      { from = src; to_ = dest }))

  let to_list t =
    List.map (edges t) ~f:(fun { from=src; to_=dest } -> (src, dest))

  let map t ~f =
    of_edges (List.flatten (List.map (edges t) ~f))

  (* BUG?: if the graph is disconnected, we'll return the connected components
   * with no cycles. *)
  let topological_order t =
    let rec add_next t ordered_nodes =
      (* All nodes with no outgoing edges. *)
      let leaves =
        List.filter (nodes t) ~f:(fun node ->
          match follow t node with
          | None | Some [] -> true
          | Some _ -> false)
      in
      match leaves with
      | [] -> ordered_nodes
      | leaves ->
        let ordered_nodes = leaves @ ordered_nodes in
        let t = List.fold leaves ~init:t ~f:remove_node in
        add_next t ordered_nodes
    in
    match add_next t [] with
    | [] -> None
    | order -> Some order

  let dump t node_to_string =
    List.map (Map.to_alist t) ~f:(fun (node, edge_info) ->
      (node_to_string node)
      , `In (List.map ~f:node_to_string (Edge_info.in_ edge_info))
      , `Out (List.map ~f:node_to_string (Edge_info.out edge_info)))
end
