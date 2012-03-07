module Module_environment : sig
  type t

  type env = t

  module Value : sig
    type t =
      | Named of string
      | Anonymous of env
  end

  (* External corresponds to a name that we know is an external dependency.
   *   if eg, let module M = List (* lookup M gives external List *)
   *
   * ... where the string list corresponds to the list of opened/included
   * external modules.
   *
   * (lookup [] "List") -> External ("List", [])
   * (lookup [open Util;;] "List") -> External ("List", ["Util"])
   * (lookup [open Util;; module M = List] "M") -> External ("List", ["Util"])
   * (lookup [open Util;; open Core.Std;;] "Foo") ->
   *   External ("Foo", ["Code.Std"; "Util"])
   * (lookup [module M = struct end] "M") -> Local []
   * (lookup [module M = struct end] "M.X") -> No_such_module
   *
   * Local of t means the module is locally defined, with a known environment.
   *
   * No such module means we know for _sure_ that the module you've asked for is
   * not in the given environment (this would be the case if the module
   * environment corresponds to the interface of some module that is statically
   * known).
   *)
  val lookup
    :  t
    -> string
    -> [ `External of string * string list
       | `Local of t
       | `No_such_module ]

  (* Difference between open and include is that open basically provides local
   * aliasing, so things within scope of the open do not need to use somethings
   * full name, whereas an include actually adds definitions:
   *
   * http://caml.inria.fr/pub/docs/manual-ocaml/manual019.html *)
  (* These functions will lookup if the included module name is known in the
   * environment. *)
  val open_module: Value.t -> t

  val include_module: Value.t -> t

  (* This is where the open/include distiction will matter? This will be for use
   * when defining a module in terms of an environment? *)
  val exported_environment: t -> t

  (* Add a module binding for a known module to this environment. *)
  val define_module: t -> string -> Value.t -> t
end

(*
module Module_info : sig
  type t

  val name: t -> string option

  val inner_module_names: t -> string list

  (* Everytime there is a module access, we look it up in our module
   * environment. *)
  (* All module accesses have to be resolved. Those that don't resolve locally
   * are interesting and represent information about external dependencies. *)
  val get_inner_module: t -> string -> [ `External | `Local of t ] option

  (* tuple containing externally included modules and externally referenced
   * modules. *)
  val external_scopes: (string list * string list) list
end
*)

(*
val modules_used: 'a Path.t -> string list

val declared_modules: 'a Path.t -> string list
*)

val module_info: Camlp4.PreCast.Ast.str_item -> Module_environment.t
