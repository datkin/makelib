include StringLabels

let is_prefix ~prefix str =
  try
    let actual_prefix = sub str ~pos:0 ~len:(length prefix) in
    actual_prefix = prefix
  with Invalid_argument _ -> false
;;

let is_suffix ~suffix str =
  try
    let actual_suffix = sub str ~pos:0 ~len:(length suffix) in
    actual_suffix = suffix
  with Invalid_argument _ -> false
;;

let split ~on:split_char str =
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
;;

let split2 split_idx str =
  let left = sub str ~pos:0 ~len:split_idx in
  let right = sub str ~pos:(split_idx+1) ~len:(length str - split_idx - 1) in
  left, right
;;

let rsplit2 ~on:split_char str =
  try
    Some (split2 (rindex str split_char) str)
  with _ -> None
;;

let lsplit2 ~on:split_char str =
  try
    Some (split2 (index str split_char) str)
  with _ -> None
;;
