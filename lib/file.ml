open Util

let stat path =
  Unix.stat (Path.to_string path)
;;

let readlink path =
  let file = Unix.readlink (Path.to_string path) in
  Path.of_string file
;;

let exists path =
  try
    Unix.access (Path.to_string path) ~perm:[ Unix.F_OK ];
    true
  with Unix.Unix_error _ -> false
;;

let list directory =
  let dir_str = Path.Dir.to_string directory in
  let (^/) = Path.(^/) in
  let dir_handle = Unix.opendir dir_str in
  let rec get files =
    try
      let file = Unix.readdir dir_handle in
      get (file :: files)
    with End_of_file ->
      (Unix.closedir dir_handle; files)
  in
  let files = get [] in
  let files = List.filter files ~f:(fun file ->
    not (file = ".") && not (file = ".."))
  in
  let paths = List.map files ~f:(fun file ->
    match (Unix.stat file).Unix.st_kind with
    | Unix.S_DIR -> directory ^/ (file ^ "/")
    | Unix.S_REG
    | Unix.S_CHR
    | Unix.S_SOCK
    | Unix.S_FIFO
    | Unix.S_LNK
    | Unix.S_BLK -> directory ^/ file)
  in
  paths
;;
