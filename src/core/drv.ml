open Gospel.Tmodule
module L = Map.Make (Gospel.Tterm.LS)
module T = Map.Make (Gospel.Ttypes.Ts)

type t = {
  module_name : string;
  stdlib : string L.t;
  repr : Derive.map;
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
let add_function ls i t = { t with functions = L.add ls i t.functions }
let find_function ls t = L.find ls t.functions
let is_function ls t = L.mem ls t.functions
let get_ls t = get_ls_env t.env
let get_ts t = get_env ns_find_ts t.env

let derive_get getter t ty =
  Option.bind (Derive.key ty) (fun k -> getter k t.repr)

let get_equality = derive_get Derive.get_equality
let add_equality k t = { t with repr = Derive.add_equality k t.repr }

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
  let base exp = Derive.(info (base exp)) in
  (* only one parameter type constructore here *)
  let forall exp = Derive.(info (forall 1 (fun x -> Builder.eapply exp x))) in
  [
    ([ "unit" ], base [%expr Repr.unit]);
    ([ "string" ], base [%expr Repr.string]);
    ([ "char" ], base [%expr Repr.char]);
    ([ "float" ], base [%expr Repr.float]);
    ([ "bool" ], base [%expr Repr.bool]);
    (* XXX FIXME not unbound integer here! *)
    ([ "integer" ], base [%expr Repr.int]);
    ([ "option" ], forall [%expr Repr.option]);
    ([ "list" ], forall [%expr Repr.list]);
    ([ "Gospelstdlib"; "seq" ], forall [%expr Repr.list]);
    ([ "Gospelstdlib"; "bag" ], forall [%expr TODO]);
    ([ "Gospelstdlib"; "ref" ], forall [%expr Repr.ref]);
    ([ "Gospelstdlib"; "array" ], forall [%expr Repr.array]);
    ([ "Gospelstdlib"; "set" ], forall [%expr TODO]);
    ([ "int" ], base [%expr Repr.int]);
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
  let open Derive in
  let repr =
    List.fold_left
      (fun acc (path, info) ->
        let key : key = get_ts_env env path |> key_from_tysymbol in
        add_info key info acc)
      empty stdlib_repr
  in
  {
    module_name;
    stdlib;
    repr;
    env;
    translations = [];
    types;
    functions = L.empty;
  }

let repr t = t.repr
let map_repr ~f t = { t with repr = f t.repr }
let map_translation ~f t = List.rev_map f t.translations
let iter_translation ~f t = List.iter f (List.rev t.translations)
let module_name t = t.module_name
