type t = string;;

let abs path = path;;

let (^/) t rel_path = Filename.concat t rel_path;;

module Map = Map.Make(String)
