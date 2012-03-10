module String = struct
  include StringLabels

  let is_prefix str ~prefix =
    try
      let actual_prefix = sub str ~pos:0 ~len:(length prefix) in
      actual_prefix = prefix
    with Invalid_argument _ -> false

  let is_suffix str ~suffix =
    try
      let actual_suffix =
        sub str ~pos:(length str - length suffix) ~len:(length suffix)
      in
      actual_suffix = suffix
    with Invalid_argument _ -> false

  let split str ~on:split_char =
    (* Split up the string from right to left. *)
    let rec collect substrs len =
      try
        let split_pos = (rindex_from str (len - 1) split_char) in
        let len = len - split_pos - 1 in
        let next_str = sub str ~pos:(split_pos + 1) ~len in
        collect (next_str :: substrs) split_pos
      with Not_found ->
        sub str ~pos:0 ~len :: substrs
    in
    collect [] (length str)

  let split2 split_idx str =
    let left = sub str ~pos:0 ~len:split_idx in
    let right = sub str ~pos:(split_idx+1) ~len:(length str - split_idx - 1) in
    left, right

  let rsplit2 t ~on:split_char =
    try
      Some (split2 (rindex t split_char) t)
    with _ -> None

  let lsplit2 t ~on:split_char =
    try
      Some (split2 (index t split_char) t)
    with _ -> None

  (* Strip all whitespace from the front and back of the string. *)
  let strip_ws t =
    let is_ws char =
      char = ' ' || char = '\t' || char = '\n'
    in
    let rec strip_forward n str =
      if n >= length str then ""
      else if is_ws str.[n] then strip_forward (n+1) str
      else sub str ~pos:n ~len:((length str) - n)
    in
    let rec strip_backward n str =
      if n < 0 then ""
      else if is_ws str.[n] then strip_backward (n-1) str
      else sub str ~pos:0 ~len:(n+1)
    in
    strip_forward 0 (strip_backward ((length t) - 1) t)

  let is_empty t =
    length t = 0

  let pp formatter t = Format.fprintf formatter "%s" t
end

module List = struct
  include ListLabels

  type 'a t = 'a list

  let init n ~f =
    let rec build m =
      if m < n then
        (f m) :: build (m+1)
      else
        []
    in
    build 0

  let is_empty t =
    match t with
    | [] -> true
    | _ :: _ -> false

  let rec last t =
    match t with
    | [] -> None
    | [x] -> Some x
    | _ :: xs -> last xs

  let fold t ~init ~f = fold_left ~f ~init t

  let map t ~f = map ~f t

  let mapi t ~f =
    let rec mapi n t acc =
      match t with
      | [] -> List.rev acc
      | x :: xs -> (mapi (n+1) xs (f n x :: acc))
    in
    mapi 0 t []

  let rec mem t ~equal x =
    match t with
    | [] -> false
    | h :: t -> equal x h || mem t ~equal x

  let remove t x ~equal =
    filter t ~f:(fun y -> not (equal x y))

  let dedupe t ~equal =
    let rec dedupe t acc =
      match t with
      | [] -> acc
      | x :: xs -> dedupe (remove xs x ~equal) (x :: acc)
    in
    List.rev (dedupe t [])

  let closure seeds ~equal ~f =
    let rec grow ~new_elts old_elts =
      let current_elts = dedupe ~equal (old_elts @ new_elts) in
      let additional_elts = flatten (map new_elts ~f) in
      let _old_ets, new_elts =
        partition additional_elts ~f:(mem current_elts ~equal)
      in
      if is_empty new_elts then
        current_elts
      else
        grow ~new_elts current_elts
    in
    grow ~new_elts:seeds []

  let to_string t ~to_string =
    let strs = map t ~f:to_string in
    "[" ^ String.concat strs ~sep:"; " ^ "]"

  let rec equal t1 t2 ~equal:elt_equal =
    match t1, t2 with
    | [], [] -> true
    | x :: t1, y :: t2 -> elt_equal x y && equal t1 t2 ~equal:elt_equal
    | [], _ :: _
    | _ :: _, [] -> false

  let filter_map t ~f =
    let rec collect t acc =
      match t with
      | [] -> List.rev acc
      | x :: xs ->
        let acc =
          match f x with
          | Some y -> y :: acc
          | None -> acc
        in
        collect xs acc
    in
    collect t []

  let partition_map t ~f =
    let rec collect t fst_acc snd_acc =
      match t with
      | [] -> List.rev fst_acc, List.rev snd_acc
      | x :: xs ->
        match f x with
        | `Fst fst -> collect xs (fst :: fst_acc) snd_acc
        | `Snd snd -> collect xs fst_acc (snd :: snd_acc)
    in
    collect t [] []

  let rec find_map t ~f =
    match t with
    | [] -> None
    | x :: xs ->
      match f x with
      | Some y -> Some y
      | None -> find_map xs ~f
end

module Non_empty_list = struct
  type 'a t = 'a * 'a list

  let create x = (x, [])

  let add (x, xs) x' = (x, x' :: xs)

  let to_list (x, xs) = x :: xs

  let hd (x, _) = x

  let split (x, xs) = x, xs
end

module Map = struct
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

  module Make(Ord: Map.OrderedType) = struct
    include Map.Make(Ord)

    let to_alist = bindings

    let keys t = List.map (to_alist t) ~f:fst
    let data t = List.map (to_alist t) ~f:snd

    let mem t key = mem key t
    let add t key data = add key data t
    let remove t key = remove key t

    let compare t1 t2 ~cmp = compare cmp t1 t2
    let equal t1 t2 ~eq = equal eq t1 t2

    let find t key =
      try
        Some (find key t)
      with
      | Not_found -> None

    let map t ~f = map f t

    let mapi t ~f =
      (* let f key data = f ~key ~data in *)
      mapi f t

    let fold t ~init ~f =
      let f key data b = f b ~key ~data in
      fold f t init

    let filter_map t ~f =
      fold t ~init:empty ~f:(fun t ~key ~data ->
        match f data with
        | Some data -> add t key data
        | None -> t)
  end
end

module Unix = struct
  include UnixLabels
end

let sprintf fmt = Printf.ksprintf (fun s -> s) fmt;;
let failwithf fmt = Printf.ksprintf (fun s () -> failwith s) fmt;;

let const a _ = a;;

let ident x = x;;
