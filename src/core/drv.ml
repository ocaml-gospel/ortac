open Gospel
module H = Hashtbl.Make (Tterm.LS)

type t = { translations : string H.t; env : Tmodule.namespace list }

let get_env get env path =
  List.find_map (fun ns -> try Some (get ns path) with Not_found -> None) env
  |> function
  | Some s -> s
  | None ->
      Fmt.(
        failwith "Internal error: path `%a' was not found"
          (list ~sep:(any ".") string)
          path)

let get_ls_env = get_env Tmodule.ns_find_ls

let v env =
  let table =
    [
      ([ "None" ], "None");
      ([ "Some" ], "Some");
      ([ "[]" ], "[]");
      ([ "infix ::" ], "(::)");
      ([ "infix =" ], "(=)");
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
  in
  let translations = H.create 0 in
  List.iter
    (fun (path, ocaml) ->
      let ls = get_ls_env env path in
      H.add translations ls ocaml)
    table;
  { translations; env }

let translate t ls = H.find_opt t.translations ls

let add_translation t ls s = H.replace t.translations ls s

let remove_translation t ls = H.remove t.translations ls

let get_ls t = get_ls_env t.env

let get_ts t = get_env Tmodule.ns_find_ts t.env
