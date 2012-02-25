val dependencies: ?exe:string -> File.t -> Module.t list

val dependency_map: ?exe:string -> File.t list -> Module.t File.Map.t
