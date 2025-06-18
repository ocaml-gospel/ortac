module W = Ortac_core.Warnings
open Ppxlib

type term = {
  txt : string;
  loc : Location.t;
  translation : (expression, W.t) result;
}

type check = {
  txt : string;
  loc : Location.t;
  translations : ((string * expression) * expression, W.t) result;
}

type invariant = {
  txt : string;
  loc : Location.t;
  translation : (string * structure_item, W.t) result;
}

type model =
  string (* the name of the model *)
  * Gospel.Identifier.Ident.t
    (* the stored generated unique name for the associated projection function *)
  * bool (* if declared as mutable*)

type type_ = {
  name : string;
  loc : Location.t;
  ghost : Gospel.Tast.ghost;
  models : model list;
  invariants : invariant list;
  equality : (expression, W.t) result;
  comparison : (expression, W.t) result;
  copy : (expression, W.t) result;
}

let type_ ~name ~loc ~ghost =
  {
    name;
    loc;
    ghost;
    models = [];
    invariants = [];
    equality = Error (W.Unsupported "equality", loc);
    comparison = Error (W.Unsupported "comparison", loc);
    copy = Error (W.Unsupported "copy", loc);
  }

type ocaml_var = {
  name : string;
  label : arg_label;
  type_ : type_;
  modified : bool;
  consumed : bool;
}

type xpost = {
  exn : string;
  args : int;
  translation : (cases, W.t list) result;
}

type value = {
  name : string;
  loc : Location.t;
  arguments : ocaml_var list;
  returns : ocaml_var list;
  register_name : string;
  model : bool;
  ghost : Gospel.Tast.ghost;
  pure : bool;
  checks : check list;
  copies : (string * expression) list;
  preconditions : term list;
  postconditions : term list;
  xpostconditions : xpost list;
}

let value ~name ~loc ~arguments ~returns ~register_name ~model ~ghost ~pure =
  {
    name;
    loc;
    arguments;
    returns;
    register_name;
    model;
    ghost;
    pure;
    checks = [];
    copies = [];
    preconditions = [];
    postconditions = [];
    xpostconditions = [];
  }

type constant = {
  name : string;
  loc : Location.t;
  type_ : type_;
  register_name : string;
  ghost : Gospel.Tast.ghost;
  checks : term list;
  invariants : expression list;
}

let constant ~name ~loc ~type_ ~register_name ~ghost =
  { name; loc; type_; register_name; ghost; checks = []; invariants = [] }

type axiom = {
  name : string;
  loc : Location.t;
  register_name : string;
  definition : term;
}

type function_ = {
  name : string;
  loc : Location.t;
  rec_ : bool;
  arguments : ocaml_var list;
  definition : term option;
}

type structure_item =
  | Type of type_
  | Value of value
  | Constant of constant
  | Function of function_
  | Predicate of function_
  | Axiom of axiom

module T = Map.Make (Gospel.Ttypes.Ts)

let stdlib_types =
  let loc = Ppxlib.Location.none in
  let ghost = Gospel.Tast.Nonghost in
  [
    ([ "unit" ], type_ ~name:"unit" ~loc ~ghost);
    ([ "string" ], type_ ~name:"string" ~loc ~ghost);
    ([ "char" ], type_ ~name:"char" ~loc ~ghost);
    ([ "float" ], type_ ~name:"float" ~loc ~ghost);
    ([ "bool" ], type_ ~name:"bool" ~loc ~ghost);
    ([ "integer" ], type_ ~name:"integer" ~loc ~ghost);
    ([ "option" ], type_ ~name:"option" ~loc ~ghost);
    ([ "list" ], type_ ~name:"list" ~loc ~ghost);
    ([ "Gospelstdlib"; "sequence" ], type_ ~name:"sequence" ~loc ~ghost);
    ([ "Gospelstdlib"; "bag" ], type_ ~name:"bag" ~loc ~ghost);
    ([ "Gospelstdlib"; "ref" ], type_ ~name:"ref" ~loc ~ghost);
    ([ "array" ], type_ ~name:"array" ~loc ~ghost);
    ([ "Gospelstdlib"; "set" ], type_ ~name:"set" ~loc ~ghost);
    ([ "int" ], type_ ~name:"int" ~loc ~ghost);
  ]

type structure = structure_item list
type types = type_ T.t
type t = { structure : structure; types : types }

let add_translation i t = { t with structure = i :: t.structure }
let add_type ts i t = { t with types = T.add ts i t.types }
let get_type ts t = T.find_opt ts t.types
let map_translation ~f t = List.rev_map f t.structure
let iter_translation ~f t = List.iter f (List.rev t.structure)

let init context =
  let types =
    List.fold_left
      (fun acc (path, type_) ->
        let ls = Ortac_core.Context.get_ts context path in
        T.add ls type_ acc)
      T.empty stdlib_types
  in
  { structure = []; types }
