open Util

type t =
  { stdout: string list
  ; stderr: string list
  ; status: Unix.process_status }
;;

let stdout t = t.stdout;;
let stderr t = t.stderr;;
let status t = t.status;;

let lines channel =
  let rec collect lines =
    try
      let line = input_line channel in
      collect (line :: lines)
    with End_of_file ->
      close_in channel; lines
  in
  List.rev (collect [])
;;

let run ?env prog args =
  let env =
    match env with
    | Some env ->
      Array.of_list (List.map env ~f:(fun (key, value) ->
        Printf.sprintf "%s=%s" key value))
    | None -> Unix.environment ()
  in
  let args = Array.of_list (prog :: args) in
  let stdin_r, stdin_w = Unix.pipe () in
  let stdout_r, stdout_w = Unix.pipe () in
  let stderr_r, stderr_w = Unix.pipe () in
  let pid =
    Unix.create_process_env ~prog ~args ~env
      ~stdin:stdin_r ~stdout:stdout_w ~stderr:stderr_w
  in
  Unix.close stdin_w;
  (* TODO: Set close on exec *)
  Unix.close stdin_r;
  Unix.close stdout_w;
  Unix.close stderr_w;
  (* TODO: Better IO processing *)
  let stdout = lines (Unix.in_channel_of_descr stdout_r) in
  let stderr = lines (Unix.in_channel_of_descr stderr_r) in
  let _pid, status = Unix.waitpid ~mode:[] pid in
  { stdout; stderr; status }
;;
