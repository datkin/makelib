let default_exe = "ocamldep" (* Just find it in the path. *)

let default_or exe =
  match exe with
  | Some exe -> exe
  | None -> default_exe
;;

let dependencies ?exe file =
  "bogus"
;;

let dependecy_map files =
  Map.File.empty
;;
