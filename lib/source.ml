open Util
open Camlp4.PreCast

module Module_environment = struct

  module String_map = Map.Make(String)

  module Exported = struct
    type t =
      { bindings: (value * int) String_map.t
      ; includes: string list }
    and value =
      | External
      | Known of t

    let empty =
      { bindings = String_map.empty
      ; includes = [] }
  end

  (* The runtime representation of a module. *)
  module Repr = struct
    type t =
      | Named of string
      | Anonymous of Exported.t
  end

  open Repr

  type t =
    { bindings: (binding * int) String_map.t
    ; includes: [ `Public of string | `Private of string ] list }
  (* This Public/Private type exists to address this situation:
   *
   * module M = struct ... end
   * open Foo (* Foo includes a module M *)
   *
   * M is still publicly exposed, but M is now shadowed locally. Yikes!
   *)
  (* The int gives the length of the includes list at the time that the
   * binding was added. Any includes added _after_ that point may shadow that
   * binding. But that binding will surely shadow anything with the same name
   * from an include _before_ that binding. *)
 and binding =
    | Public of value
    (* an optional public binding that's been shadowed. *)
    | Private of value * (value * int) option
  and value = Exported.value

  let empty =
    { bindings = String_map.empty
    ; includes = [] }

  (* To determine what is exported, first we filter the inludes and add
   * placeholders to mark the former position of the includes. Then, we take the
   * bindings, and filter out all the private bindings (restoring earlier
   * shadowed public bindings, if any), and updating the binding depths to
   * correspond with locations in the "exported" includes environment. *)
  let exported t =
    let tagged_includes =
      let num_includes = List.length t.includes in
      List.mapi t.includes ~f:(fun i inc -> num_includes - i, inc)
    in
    let tagged_includes =
      List.filter_map tagged_includes ~f:(fun (n, inc) ->
        match inc with
        | `Private _ -> None
        | `Public name -> Some (n, name))
    in
    let exported_includes = List.map tagged_includes ~f:snd in
    let translate_depth orig_depth =
      let depth_map =
        let num_includes = List.length tagged_includes in
        List.mapi tagged_includes ~f:(fun i (old_depth, _inc) ->
          (num_includes - i, old_depth))
      in
      let new_binding_depth (new_depth, old_depth) =
        if orig_depth <= old_depth then
          Some new_depth
        else None
      in
      match List.find_map depth_map ~f:new_binding_depth with
      | Some new_depth -> new_depth
      | None -> 0
    in
    let exported_bindings =
      String_map.filter_map t.bindings ~f:(fun (binding, depth) ->
        match binding with
        | Public value ->
          Some (value, translate_depth depth)
        | Private (_, Some (value, depth)) ->
          Some (value, translate_depth depth)
        | Private (_, None) -> None)
    in
    { Exported.bindings = exported_bindings
    ; includes = exported_includes }

  let new_binding ~depth ~is_private =
    if is_private
    then fun value -> (Public value, depth)
    else fun value -> (Private (value, None), depth)

  let lookup t name =
    let bound_value, take_includes =
      match String_map.find t.bindings name with
      | Some (Public value, depth)
      | Some (Private (value, _), depth) ->
        Some value, List.length t.includes - depth
      | None ->
        None, 0
    in
    let includes =
      List.map t.includes ~f:(fun inc ->
        match inc with
        | `Public name -> name
        | `Private name -> name)
    in
    let after, before = List.divide includes ~at:take_includes in
    let module Ex = Exported in
    match bound_value, after, before with
    (* CR datkin: is this right? *)
    | Some Ex.External, _, _ -> `External (name, before)
    | Some (Ex.Known exported), [], _ -> `Local exported
    | Some (Ex.Known exported), inc :: others, _ ->
      let incs = Non_empty_list.of_split inc others in
      `Ambiguous (exported, incs)
    (* CR datkin: This assumes we include stdlib in some base env. *)
    | None, [], [] -> `Unbound
    (* CR datkin: invariant, after should be empty if bound_value is none.
     * How to enforce this? *)
    | None, [], _ -> `External (name, before)
    | None, _ :: _, _ -> assert false

  let open_module t repr =
    match repr with
    | Repr.Named name ->
      let 
        (* Resolve the name. If it
    | Repr.Exported ex

    (*
  let merge ~base:t ~new_:new_t ~is_private =
    let current_depth = List.length t.includes in
    let merge_one =
      if is_private then
        fun (new_value, depth) (current_binding, current_binding_depth) ->
          let depth = depth + current_binding_depth
          let new_binding =
            match current_binding with
            | Public current_value -> Private (value, Some current_value)
            | Private (_, shadowed_value) -> Private (new_value, shadowed_value)
          in
      else
        fun (new_value, depth) ->
          let depth = current_depth + depth in
          const (new_binding ~depth ~is_private new_value)
    in
    let new_binding = new_binding ~is_private in
    let merge_all (name, new_value) bindings =
      try
        let current_binding = String_map.find name bindings in
        String_map.add name (merge_one new_value current_binding) bindings
      with
      | Not_found ->
        String_map.add name (new_binding new_value) bindings
    in
    List.fold (public_values new_bindings) ~init:base_bindings ~f:merge

  let get_value t repr =
    match repr with
    | Named name ->
      begin try
        Known (String_map.find name t)
      with
      | Not_found -> External
      end
    | Anonymous env -> Known env

  let generic_include t repr ~is_private =
    match get_value r repr with
    | Known env -> merge ~base:t ~new_:env ~is_private
    (* This bit is wrong *)
    | External -> String_map.add name (new_binding ~is_private External) t

  (* Opening or including any module basically erases any knowledge we have
   * about how future bindings in the module may resolve...
   *
   * module M = ...
   * include X
   *
   * Once X has been included, it's _possible that further references to M
   * should be resolved through X, although if they can't be resolved through
   * X, they are satisfied by M.
   *
   * However, for the purposes of dependency resolution, it may be okay to
   * fudge...? That is to say, we know we'll have to build X first b/c it's
   * included. If it happens to redefine M, fine!
   *
   * The only problem would occur if we later had a reference to something
   * like [M.Foo.some_value]. If we don't acknolwedge that M has been rebound,
   * and the fold M binding (explicitly defined on the first line in the
   * example) is a closed ref, then we may interpret the M.Foo.some_value
   * refeence as an error, when it's possible that we're just expecting X to
   * contain M.Foo.some_value.
   *)
  let open_module t repr =
    generic_include t repr ~is_private:true

  let include_module t value =
    generic_include t repr ~is_private:false

  let define_module t name value =
    match get_value r repr with
    | 

  (* TODO *)
  let exported_environment t = t

  let lookup t name =
    let { opens; includes; bindings } = t in
  *)

end

module Env = Module_environment;;

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
(*
let modules_used path =
  let module M = Camlp4OCamlRevisedParser.Make(Syntax) in
  let module N = Camlp4OCamlParser.Make(Syntax) in
  let stream = Stream.of_channel (open_in (Path.to_string path)) in
  let file_loc = Loc.mk (Path.to_string path) in
  ignore (stream, file_loc)
;;

let declared_modules path =
  ignore path; []
;;
*)

let module_info str_item =
  ignore str_item;
  Env.empty
;;
