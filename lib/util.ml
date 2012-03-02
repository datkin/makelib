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

  let is_empty t =
    length t = 0
end

module List = struct
  include ListLabels

  type 'a t = 'a list

  let init n ~f =
    let rec build m =
      if m < n then
        (f n) :: build (m+1)
      else
        []
    in
    build 0

  let rec last t =
    match t with
    | [] -> None
    | [x] -> Some x
    | _ :: xs -> last xs

  let fold t ~init ~f = fold_left ~f ~init t

  let map t ~f = map ~f t

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
    (* TODO: I guess we could just stop when next is empty? *)
    let rec grow ~seeds acc =
      let next = flatten (map seeds ~f) in
      let acc' = dedupe ~equal (acc @ seeds @ next) in
      if length acc = length acc' then
        acc
      else
        (* TODO!!!!: mem should use [equal] argument. *)
        let _old, new_seeds = partition acc' ~f:(mem ~set:acc) in
        grow ~seeds:new_seeds acc'
    in
    grow ~seeds []
end

module Unix = struct
  include UnixLabels
end

let failwithf fmt = Printf.ksprintf (fun s () -> failwith s) fmt;;

let const a _ = a;;
