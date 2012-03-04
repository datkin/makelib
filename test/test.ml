open Util

module Result = struct
  type t =
    | Pass
    | Fail of string

  let passed t =
    match t with
    | Pass -> true
    | Fail _ -> false

  let to_string t =
    match t with
    | Pass -> "passed"
    | Fail message -> Printf.sprintf "failed: %s" message
end

module Assert = struct
  let (<&&>) r1 r2 =
    match r1 with
    | Result.Pass -> r2
    | Result.Fail msg -> Result.Fail msg

  let (<||>) r1 r2 =
    match r1 with
    | Result.Pass -> Result.Pass
    | Result.Fail _ -> r2

  let expect ?(eq=(=)) ?to_string ~is:expected actual =
    if eq expected actual then
      Result.Pass
    else
      let message =
        match to_string with
        | Some to_string ->
          Printf.sprintf "Expected %s, got %s" (to_string expected) (to_string actual)
        | None -> "Values not equal"
      in
      Result.Fail message

  let is_true ?msg bool =
    if bool then
      Result.Pass
    else
      match msg with
      | Some msg -> Result.Fail msg
      | None -> Result.Fail "Expected true"

  let is_false ?msg bool =
    if not bool then
      Result.Pass
    else
      match msg with
      | Some msg -> Result.Fail msg
      | None -> Result.Fail "Expected false"

  let list_eq ?(eq=(=)) ?to_string l1 l2 =
    let to_string =
      match to_string with
      | Some to_string -> Some (List.to_string ~to_string)
      | None -> None
    in
    expect l2 ~is:l1 ~eq:(List.equal ~equal:eq) ?to_string

  let string_eq s1 s2 =
    expect s2 ~is:s1 ~eq:(=) ~to_string:(fun x -> x)

  let int_eq i1 i2 =
    expect i2 ~is:i1 ~eq:(=) ~to_string:string_of_int
end

type t =
  { name: string
  ; test: unit -> Result.t }
;;

let make name f =
  { name; test = f }
;;

let run t =
  try
    t.test ()
  with exn ->
    let message = Printf.sprintf "Failed with: %s" (Printexc.to_string exn) in
    Result.Fail message
;;

let run_all_and_report ts =
  let results = List.map ts ~f:(fun t -> t.name, run t) in
  let passed, failed =
    List.partition results ~f:(fun (_name, result) -> Result.passed result)
  in
  List.iter failed ~f:(fun (name, result) ->
    Printf.printf "%s %s\n" name (Result.to_string result));
  Printf.printf "%d/%d tests passed\n" (List.length passed) (List.length ts);
  if List.is_empty failed then exit 0
  else exit 1
;;
