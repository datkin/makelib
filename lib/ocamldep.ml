open Util

let default_exe = "ocamldep" (* Just find it in the path. *)

let default_or exe =
  match exe with
  | Some exe -> exe
  | None -> default_exe
;;

let parse_line line =
  match String.rsplit2 ~on:';' line with
  | Some (file, deps) ->
    let deps = String.split deps ~on:' ' in
    file, List.filter deps ~f:String.is_empty
  | None -> failwithf "Could not parse ocamldep line: %s" line ()
;;

let dependencies ?exe file =
  let exe = default_or exe in
  let file_name = Path.to_abs file in
  let _process = Process.run exe ["-modules"; file_name] in
  []
;;

let dependency_map ?exe files =
  Path.Map.empty
;;
