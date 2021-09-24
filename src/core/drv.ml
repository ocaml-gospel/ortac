type t = {
  translations : (Gospel.Tterm.lsymbol, string) Hashtbl.t;
  env : Gospel.Tmodule.namespace list;
}

let get_env get env path =
  List.find_map (fun ns -> try Some (get ns path) with Not_found -> None) env
  |> function
  | Some s -> s
  | None ->
      Fmt.(
        failwith "Internal error: path `%a' was not found"
          (list ~sep:(any ".") string)
          path)

let get_ls_env = get_env Gospel.Tmodule.ns_find_ls

let v env =
  let table =
    [
      ([ "None" ], "None");
      ([ "Some" ], "Some");
      ([ "[]" ], "[]");
      ([ "infix ::" ], "(::)");
      ([ "infix =" ], "(=)");
      ([ "Gospelstdlib"; "infix +" ], "Z.add");
      ([ "Gospelstdlib"; "infix -" ], "Z.sub");
      ([ "Gospelstdlib"; "infix *" ], "Z.mul");
      ([ "Gospelstdlib"; "infix /" ], "Z.div");
      ([ "Gospelstdlib"; "mod" ], "Z.rem");
      ([ "Gospelstdlib"; "pow" ], "Z.pow");
      ([ "Gospelstdlib"; "prefix -" ], "Z.neg");
      ([ "Gospelstdlib"; "infix >" ], "Z.gt");
      ([ "Gospelstdlib"; "infix >=" ], "Z.geq");
      ([ "Gospelstdlib"; "infix <" ], "Z.lt");
      ([ "Gospelstdlib"; "infix <=" ], "Z.leq");
      ([ "Gospelstdlib"; "integer_of_int" ], "Z.of_int");
      ([ "Gospelstdlib"; "abs" ], "Z.abs");
      ([ "Gospelstdlib"; "min" ], "Z.min");
      ([ "Gospelstdlib"; "max" ], "Z.max");
      ([ "Gospelstdlib"; "succ" ], "Z.succ");
      ([ "Gospelstdlib"; "pred" ], "Z.pred");
      ([ "Gospelstdlib"; "Array"; "length" ], "Array.length");
      ([ "Gospelstdlib"; "Array"; "get" ], "Array.get");
      ([ "Gospelstdlib"; "Array"; "for_all" ], "Array.for_all");
    ]
  in
  let translations = Hashtbl.create 0 in
  List.iter
    (fun (path, ocaml) ->
      let ls = get_ls_env env path in
      Hashtbl.add translations ls ocaml)
    table;
  { translations; env }

let translate t ls = Hashtbl.find_opt t.translations ls

let get_ls t = get_ls_env t.env

let get_ts t = get_env Gospel.Tmodule.ns_find_ts t.env
