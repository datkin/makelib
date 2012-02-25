val find
  :  ?max_depth:int
  -> ?follow_symlinks:unit
  -> ?filter:(string * File.stats -> bool)
  -> string
  -> (string * File.stats) list
