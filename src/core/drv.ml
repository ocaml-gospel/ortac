type t = {
  translations : (Gospel.Tterm.lsymbol, string) Hashtbl.t;
  env : Gospel.Tmodule.namespace list;
}

let get_ls_env env path =
  List.find_map
    (fun ns ->
      try Some (Gospel.Tmodule.ns_find_ls ns path) with Not_found -> None)
    env
  |> function
  | Some ls -> ls
  | None ->
      Fmt.(
        failwith
          "Internal error: path `%a' was not found when initialising ortac \
           driver"
          (list ~sep:(any ".") string)
          path)

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
