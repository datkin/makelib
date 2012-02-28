val dependencies: ?exe:'a Path.t -> 'b Path.t -> Module.t list

val dependency_map: ?exe:'a Path.t -> 'b Path.t list -> ('b Path.t * Module.t list) list
