let files directory =
  let dir_handle = Unix.opendir directory in
  let rec get files =
    try
      let file = Unix.readdir dir_handle in
      get (file :: files)
    with End_of_file ->
      (Unix.closedir dir_handle; files)
  in
  get []
;;
