open Gospel.Tmodule
module L = Map.Make (Gospel.Tterm.LS)
module T = Map.Make (Gospel.Ttypes.Ts)

type t = {
  module_name : string;
  stdlib : string L.t;
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
let translate_stdlib ls t = L.find_opt ls t.stdlib
let add_translation i t = { t with translations = i :: t.translations }
let add_type ts i t = { t with types = T.add ts i t.types }
let get_type ts t = T.find_opt ts t.types
let add_function ls i t = { t with functions = L.add ls i t.functions }
let find_function ls t = L.find ls t.functions
let is_function ls t = L.mem ls t.functions
let get_ls t = get_ls_env t.env
let get_ts t = get_env ns_find_ts t.env

let stdlib =
  [
    ([ "None" ], "None");
    ([ "Some" ], "Some");
    ([ "[]" ], "[]");
    ([ "infix ::" ], "(::)");
    ([ "infix =" ], "(=)");
    ([ "Gospelstdlib"; "infix +" ], "Runtime.Z.add");
    ([ "Gospelstdlib"; "infix -" ], "Runtime.Z.sub");
    ([ "Gospelstdlib"; "infix *" ], "Runtime.Z.mul");
    ([ "Gospelstdlib"; "infix /" ], "Runtime.Z.div");
    ([ "Gospelstdlib"; "mod" ], "Runtime.Z.rem");
    ([ "Gospelstdlib"; "pow" ], "Runtime.Z.pow");
    ([ "Gospelstdlib"; "logand" ], "Runtime.Z.logand");
    ([ "Gospelstdlib"; "prefix -" ], "Runtime.Z.neg");
    ([ "Gospelstdlib"; "infix >" ], "Runtime.Z.gt");
    ([ "Gospelstdlib"; "infix >=" ], "Runtime.Z.geq");
    ([ "Gospelstdlib"; "infix <" ], "Runtime.Z.lt");
    ([ "Gospelstdlib"; "infix <=" ], "Runtime.Z.leq");
    ([ "Gospelstdlib"; "integer_of_int" ], "Runtime.Z.of_int");
    ([ "Gospelstdlib"; "abs" ], "Runtime.Z.abs");
    ([ "Gospelstdlib"; "min" ], "Runtime.Z.min");
    ([ "Gospelstdlib"; "max" ], "Runtime.Z.max");
    ([ "Gospelstdlib"; "succ" ], "Runtime.Z.succ");
    ([ "Gospelstdlib"; "pred" ], "Runtime.Z.pred");
    ([ "Gospelstdlib"; "Array"; "make" ], "Runtime.Array.make");
    ([ "Gospelstdlib"; "Array"; "length" ], "Runtime.Array.length");
    ([ "Gospelstdlib"; "Array"; "get" ], "Runtime.Array.get");
    ([ "Gospelstdlib"; "Array"; "for_all" ], "Runtime.Array.for_all");
  ]

let init module_name env =
  let stdlib =
    List.fold_left
      (fun acc (path, ocaml) ->
        let ls = get_ls_env env path in
        L.add ls ocaml acc)
      L.empty stdlib
  in
  {
    module_name;
    stdlib;
    env;
    translations = [];
    types = T.empty;
    functions = L.empty;
  }

let map_translation ~f t = List.rev_map f t.translations
let iter_translation ~f t = List.iter f (List.rev t.translations)
let module_name t = t.module_name
