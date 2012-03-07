open Util
open Camlp4.PreCast

(* We have a module environment when we are eval'ing a module. It's very simple
 * -- it binds a module name to a module type. If a name isn't in the
 * environment, it's an external module. The module def'n itself closes over a
 * module environment. For accessing a submodule we need to access this
 * subenvironment. *)

module Module_info = struct
  type scope =
    { }

  type t =
    { name: string option
    ; inner_modules: t list (* only inner _exposed_ modules? This could probably
    be a combination of known and unknown modules. *)
    ; scopes: 
    }


end

type module_ =
  { name: string
  (* Order matters -- if names are re-used, shadowing occurs. *)
  ; inner_modules: export list }
;;

type import = {
  (* Order matters -- multiple imports may cause shadowing. *)
  { opened_modules: name list
  ; used_modules: name list
  (* Any import scopes nested inside of this one. *)
  ; scopes: import list }

(* In order to do proper dependency resolution, we need to do determine two
 * things for each ml file (module) we compile:
 *  - which modules does it export?
 *  - which external modules does it use (which modules are imported)?
 *
 * For exported modules, we need to know all inner modules as well.
 *
 * For imported modules, we need to know what external modules were opened in
 * scope of the modules use.
 *
 * It is not important to distinguish between modules and functors b/c they
 * share the same namespace. Though it is necessary to track all nested,
 * exported modules, it's not necessary to discover accesses to nested modules
 * (ie, Foo.Bar.x need only be reported as an access of "Foo", not "Foo.Bar").
 * However, it _is_ significant if an inner module is opened to note this.
 *
 * We can determine this with an abstract interpretation of modules -- we
 * basically must evaluate each module definition to determine which modules it
 * contains, expanding those imports -
 * The module flow we care about should all be statically decidable: module
 * values are always governed by a statically known interface.
 *
 * Doing this is simple. Evaluate the module with a module environment.
 * The only things added to the module environment are module definitions
 * introduced in the file. Any other module reference is an external
 * reference.
 *
 * It's important to know what the constraints are on a module. Eg.
 *
 *   module X = struct
 *     include Y
 *   end
 *   open X
 *
 * is different from:
 *
 *   module X : sig end = struct
 *     include Y
 *   end
 *   open X
 *
 * In the first case, when we open X, we should note that we are essentially
 * also opening external module Y. In the second case, when we open X, we don't
 * necessarily open _any_ external modules. Unless of course the signature has
 * an include in it.
 *)

(* Docs: http://camlunity.ru/doc/camlp4-3.12/Camlp4.html *)
(* http://camlunity.ru/doc/camlp4-3.12/Camlp4.Sig.Camlp4Ast.html *)
(* See also:
 * http://ambassadortothecomputers.blogspot.com/2009/01/reading-camlp4-part-4-consuming-ocaml.html
 * AND!!!
 * http://ambassadortothecomputers.blogspot.com/2008/12/reading-camlp4-part-1-ocaml-ast.html
 *
 * Ast.IdUid uppercase id (ie module name, c'tor)
 * Ast.IdLid lowercase id
 * Ast.StOpn (uid)
 * Ast.
 *)
let modules_used path =
  let module M = Camlp4OCamlRevisedParser.Make(Syntax) in
  let module N = Camlp4OCamlParser.Make(Syntax) in
  let stream = Stream.of_channel (open_in (Path.to_string path)) in
  let file_loc = Loc.mk (Path.to_string path) in
;;

let declared_modules path =
  ignore path; []
;;
