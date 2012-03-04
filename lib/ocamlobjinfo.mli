val units: ?exe:'a Path.t -> 'b Path.t -> Module.t list

val c_flags: ?exe:'a Path.t -> 'b Path.t -> string list
val c_objs: ?exe:'a Path.t -> 'b Path.t -> string list
