type t =
  { stdout: string list
  ; stderr: string list
  ; status: Unix.process_status }
;;

let lines channel =
  let rec collect lines =
    try
      let line = input_line channel in
      Printf.printf "got line: %s\n%!" line;
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
      Array.of_list (List.map (fun (key, value) ->
        Printf.sprintf "%s=%s" key value) env)
    | None -> Unix.environment ()
  in
  let args = Array.of_list (prog :: args) in
  let stdin_r, stdin_w = Unix.pipe () in
  let stdout_r, stdout_w = Unix.pipe () in
  let stderr_r, stderr_w = Unix.pipe () in
  let pid =
    UnixLabels.create_process_env ~prog ~args ~env
      ~stdin:stdin_r ~stdout:stdout_w ~stderr:stderr_w
  in
  Unix.close stdin_r;
  Unix.close stdout_w;
  Unix.close stderr_w;
  let stdout = lines (Unix.in_channel_of_descr stdout_r) in
  let stderr = lines (Unix.in_channel_of_descr stderr_r) in
  let _pid, status = UnixLabels.waitpid ~mode:[] pid in
  { stdout; stderr; status }
;;
