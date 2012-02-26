val dependencies: ?exe:string -> Path.t -> Module.t list

val dependency_map: ?exe:string -> Path.t list -> Module.t Path.Map.t
