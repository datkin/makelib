type t =
  { stdout: string list
  ; stderr: string list
  ; status: Unix.process_status }

val run: ?env:(string * string) list -> string -> string list -> t

val stdout: t -> string list

val stderr: t -> string list

val status: t -> Unix.process_status
