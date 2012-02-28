open Util

let default_exe = "ocamldep" (* Just find it in the path. *)

let default_or exe =
  match exe with
  | Some exe -> Path.to_string exe
  | None -> default_exe
;;

let parse_line path line =
  match String.rsplit2 ~on:':' line with
  | Some (filename, deps) ->
    if Path.to_string path = filename then
      let deps = String.split deps ~on:' ' in
      let deps = List.filter deps ~f:((<>) "") in
      path, List.map deps ~f:Module.of_string
    else
      failwithf "Expected dependencies for %s, got %s"
        (Path.to_string path) filename ()
  | None -> failwithf "Could not parse ocamldep line: %s" line ()
;;

let dependency_map ?exe paths =
  let exe = default_or exe in
  let path_strs = List.map paths ~f:Path.to_string in
  let process = Process.run exe ("-modules" :: path_strs) in
  match Process.status process, Process.stdout process with
  | Unix.WEXITED 0, lines ->
    List.map2 paths lines ~f:parse_line
  | Unix.WEXITED code, _ ->
    failwithf "ocamldep exited with code %d" code ()
  | Unix.WSIGNALED _, _ -> failwithf "ocamldep signaled" ()
  | Unix.WSTOPPED _, _ -> failwithf "ocamldep stopped" ()
;;

let dependencies ?exe path =
  match dependency_map ?exe [ path ] with
  | [ _, deps ] -> deps
  | [] ->
    failwithf "ocamldep exited normally, but with no output, is %s an .ml/.mli file?"
      (Path.to_string path) ()
  | _ ->
    failwithf "ocamldep produced too much output!?" ()
;;
