open Gospel.Tmodule
module L = Map.Make (Gospel.Tterm.LS)
module T = Map.Make (Gospel.Ttypes.Ts)

module Repr : sig
  type t

  val repr : [ `B | `F ] -> Ppxlib.expression -> string -> t
  val get_repr : t -> Ppxlib.expression option
  val get_equality : t -> string option
  val get_comparison : t -> string option
  val get_name : t -> string option
end = struct
  type 'a kind = Base of 'a | Forall of ('a list -> 'a)
  type repr_expr = Ppxlib.expression kind
  type repr_symbol = string kind

  let kind_to_option = function Base a -> Some a | Forall _ -> None

  (* make a module! *)
  type t = {
    expr : repr_expr;
    name : repr_symbol;
    equal : repr_symbol;
    compare : repr_symbol;
  }

  let repr (kind : [ `B | `F ]) expr prefix =
    let open Ppxlib in
    match kind with
    | `B ->
        {
          expr = Base expr;
          name = Base (gen_symbol ~prefix:("__" ^ prefix) ());
          equal = Base (gen_symbol ~prefix:("__" ^ prefix ^ "_equal") ());
          compare = Base (gen_symbol ~prefix:("__" ^ prefix ^ "_compare") ());
        }
    | `F ->
        let mk_prefix args = String.concat "_" ("_" :: prefix :: args) in
        {
          expr = Forall (fun r -> Builder.eapply expr r);
          name = Forall (fun args -> gen_symbol ~prefix:(mk_prefix args) ());
          equal =
            Forall
              (fun args -> gen_symbol ~prefix:(mk_prefix args ^ "_equal") ());
          compare =
            Forall
              (fun args -> gen_symbol ~prefix:(mk_prefix args ^ "_compare") ());
        }

  let get_repr repr = kind_to_option repr.expr
  let get_equality repr = kind_to_option repr.equal
  let get_name repr = kind_to_option repr.name
  let get_comparison repr = kind_to_option repr.compare
end

type repr = Repr.t

type t = {
  module_name : string;
  stdlib : string L.t;
  repr : Repr.t T.t;
  env : namespace;
  translations : Translated.structure_item list;
  types : Translated.type_ T.t;
  functions : string L.t;
}

let get_env get ns path =
  try get ns path
  with Not_found ->
    Fmt.(
      failwith "Internal error: path `%a' was not found"
        (list ~sep:(any ".") string)
        path)

let get_ls_env = get_env ns_find_ls
let get_ts_env = get_env ns_find_ts
let translate_stdlib ls t = L.find_opt ls t.stdlib
let add_translation i t = { t with translations = i :: t.translations }
let add_type ts i t = { t with types = T.add ts i t.types }
let get_type ts t = T.find_opt ts t.types
let add_repr ts r t = { t with repr = T.add ts r t.repr }
let get_repr ts t = T.find_opt ts t.repr
let add_function ls i t = { t with functions = L.add ls i t.functions }
let find_function ls t = L.find ls t.functions
let is_function ls t = L.mem ls t.functions
let get_ls t = get_ls_env t.env
let get_ts t = get_env ns_find_ts t.env
let bind_repr f ts t = Option.bind (get_repr ts t) f
let get_repr_name = bind_repr Repr.get_name
let get_repr_equality = bind_repr Repr.get_equality
let get_repr_comparison = bind_repr Repr.get_comparison
let get_repr_expr = bind_repr Repr.get_repr

let stdlib_types =
  let open Translated in
  let loc = Ppxlib.Location.none in
  [
    ([ "unit" ], type_ ~name:"unit" ~loc ~mutable_:Immutable ~ghost:false);
    ([ "string" ], type_ ~name:"string" ~loc ~mutable_:Immutable ~ghost:false);
    ([ "char" ], type_ ~name:"char" ~loc ~mutable_:Immutable ~ghost:false);
    ([ "float" ], type_ ~name:"float" ~loc ~mutable_:Immutable ~ghost:false);
    ([ "bool" ], type_ ~name:"bool" ~loc ~mutable_:Immutable ~ghost:false);
    ([ "integer" ], type_ ~name:"integer" ~loc ~mutable_:Immutable ~ghost:false);
    ( [ "option" ],
      type_ ~name:"option" ~loc
        ~mutable_:(Dependant (function [ m ] -> m | _ -> assert false))
        ~ghost:false );
    ( [ "list" ],
      type_ ~name:"list" ~loc
        ~mutable_:(Dependant (function [ m ] -> m | _ -> assert false))
        ~ghost:false );
    ( [ "Gospelstdlib"; "seq" ],
      type_ ~name:"seq" ~loc
        ~mutable_:(Dependant (function [ m ] -> m | _ -> assert false))
        ~ghost:false );
    ( [ "Gospelstdlib"; "bag" ],
      type_ ~name:"bag" ~loc
        ~mutable_:(Dependant (function [ m ] -> m | _ -> assert false))
        ~ghost:false );
    ( [ "Gospelstdlib"; "ref" ],
      type_ ~name:"ref" ~loc
        ~mutable_:(Dependant (fun _ -> Mutable))
        ~ghost:false );
    ( [ "Gospelstdlib"; "array" ],
      type_ ~name:"array" ~loc
        ~mutable_:(Dependant (fun _ -> Mutable))
        ~ghost:false );
    ( [ "Gospelstdlib"; "set" ],
      type_ ~name:"set" ~loc
        ~mutable_:(Dependant (function [ m ] -> m | _ -> assert false))
        ~ghost:false );
    ([ "int" ], type_ ~name:"int" ~loc ~mutable_:Immutable ~ghost:false);
  ]

let stdlib =
  [
    ([ "None" ], "None");
    ([ "Some" ], "Some");
    ([ "[]" ], "[]");
    ([ "infix ::" ], "(::)");
    ([ "infix =" ], "(=)");
    ([ "prefix !" ], "!");
    ([ "Gospelstdlib"; "infix +" ], "Ortac_runtime.Z.add");
    ([ "Gospelstdlib"; "infix -" ], "Ortac_runtime.Z.sub");
    ([ "Gospelstdlib"; "infix *" ], "Ortac_runtime.Z.mul");
    ([ "Gospelstdlib"; "infix /" ], "Ortac_runtime.Z.div");
    ([ "Gospelstdlib"; "mod" ], "Ortac_runtime.Z.rem");
    ([ "Gospelstdlib"; "pow" ], "Ortac_runtime.Z.pow");
    ([ "Gospelstdlib"; "logand" ], "Ortac_runtime.Z.logand");
    ([ "Gospelstdlib"; "prefix -" ], "Ortac_runtime.Z.neg");
    ([ "Gospelstdlib"; "infix >" ], "Ortac_runtime.Z.gt");
    ([ "Gospelstdlib"; "infix >=" ], "Ortac_runtime.Z.geq");
    ([ "Gospelstdlib"; "infix <" ], "Ortac_runtime.Z.lt");
    ([ "Gospelstdlib"; "infix <=" ], "Ortac_runtime.Z.leq");
    ([ "Gospelstdlib"; "integer_of_int" ], "Ortac_runtime.Z.of_int");
    ([ "Gospelstdlib"; "abs" ], "Ortac_runtime.Z.abs");
    ([ "Gospelstdlib"; "min" ], "Ortac_runtime.Z.min");
    ([ "Gospelstdlib"; "max" ], "Ortac_runtime.Z.max");
    ([ "Gospelstdlib"; "succ" ], "Ortac_runtime.Z.succ");
    ([ "Gospelstdlib"; "pred" ], "Ortac_runtime.Z.pred");
    ([ "Gospelstdlib"; "Array"; "make" ], "Ortac_runtime.Array.make");
    ([ "Gospelstdlib"; "Array"; "length" ], "Ortac_runtime.Array.length");
    ([ "Gospelstdlib"; "Array"; "get" ], "Ortac_runtime.Array.get");
    ([ "Gospelstdlib"; "Array"; "for_all" ], "Ortac_runtime.Array.for_all");
  ]

let stdlib_repr =
  let loc = Ppxlib.Location.none in
  let open Repr in
  [
    ([ "unit" ], repr `B [%expr Repr.unit] "unit");
    ([ "string" ], repr `B [%expr Repr.string] "string");
    ([ "char" ], repr `B [%expr Repr.char] "char");
    ([ "float" ], repr `B [%expr Repr.float] "float");
    ([ "bool" ], repr `B [%expr Repr.bool] "bool");
    (* XXX FIXME not unbound integer here! *)
    ([ "integer" ], repr `B [%expr Repr.int] "integer");
    ([ "option" ], repr `F [%expr Repr.option] "option");
    ([ "list" ], repr `F [%expr Repr.list] "list");
    ([ "Gospelstdlib"; "seq" ], repr `F [%expr Repr.list] "seq");
    ([ "Gospelstdlib"; "bag" ], repr `F [%expr TODO] "bag");
    ([ "Gospelstdlib"; "ref" ], repr `F [%expr Repr.ref] "ref");
    ([ "Gospelstdlib"; "array" ], repr `F [%expr Repr.array] "array");
    ([ "Gospelstdlib"; "set" ], repr `F [%expr TODO] "set");
    ([ "int" ], repr `B [%expr Repr.int] "int");
  ]

let init module_name env =
  let stdlib =
    List.fold_left
      (fun acc (path, ocaml) ->
        let ls = get_ls_env env path in
        L.add ls ocaml acc)
      L.empty stdlib
  in
  let types =
    List.fold_left
      (fun acc (path, type_) ->
        let ls = get_ts_env env path in
        T.add ls type_ acc)
      T.empty stdlib_types
  in
  let repr =
    List.fold_left
      (fun acc (path, repr) ->
        let ls = get_ts_env env path in
        T.add ls repr acc)
      T.empty stdlib_repr
  in
  {
    module_name;
    stdlib;
    env;
    translations = [];
    types;
    repr;
    functions = L.empty;
  }

let map_translation ~f t = List.rev_map f t.translations
let iter_translation ~f t = List.iter f (List.rev t.translations)
let module_name t = t.module_name
