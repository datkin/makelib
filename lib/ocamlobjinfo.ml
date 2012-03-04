open Util

let default_exe = "ocamlobjinfo" (* Just find it in the path. *)

let default_or exe =
  match exe with
  | Some exe -> Path.to_string exe
  | None -> default_exe
;;

let get_lines select_key lines =
  let rec collect lines acc =
    match lines with
    | [] -> acc
    | line :: lines ->
      let acc =
        match String.lsplit2 line ~on:':' with
        | Some (key, value) ->
          if key = select_key then (String.strip_ws value) :: acc
          else acc
        | None -> acc
      in
      collect lines acc
  in
  collect lines []
;;

let with_output ?exe path ~f =
  let exe = default_or exe in
  let process = Process.run exe [Path.to_string path] in
  match Process.status process, Process.stdout process with
  | Unix.WEXITED 0, lines -> f lines
  | Unix.WEXITED code, _ ->
    failwithf "ocamlobjinfo exited with code %d" code ()
  | Unix.WSIGNALED _, _ -> failwithf "ocamlobjinfo signaled" ()
  | Unix.WSTOPPED _, _ -> failwithf "ocamlobjinfo stopped" ()

let units ?exe path =
  List.map (with_output ?exe path ~f:(get_lines "Unit name")) ~f:Module.of_string
;;

let c_flags ?exe path =
  with_output ?exe path ~f:(get_lines "Extra C options")
;;

let c_objs ?exe path =
  with_output ?exe path ~f:(get_lines "Extra C object files")
;;
