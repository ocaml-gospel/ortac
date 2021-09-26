open Gospel
module Hls = Hashtbl.Make (Tterm.LS)

module Hts = Hashtbl.Make (struct
  include Ttypes.Ts

  let hash = Hashtbl.hash
end)

type t = {
  type_defs : Tast.type_kind Hts.t;
  translations : string Hls.t;
  env : Tmodule.namespace list;
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

let get_ls_env = get_env Tmodule.ns_find_ls

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
      ([ "Gospelstdlib"; "logand" ], "Z.logand");
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
  let translations = Hls.create 0 in
  List.iter
    (fun (path, ocaml) ->
      let ls = get_ls_env env path in
      Hls.add translations ls ocaml)
    table;
  { type_defs = Hts.create 0; translations; env }

let translate t ls = Hls.find_opt t.translations ls

let add_translation t ls s = Hls.replace t.translations ls s

let remove_translation t ls = Hls.remove t.translations ls

let get_ls t = get_ls_env t.env

let get_ts t = get_env Tmodule.ns_find_ts t.env

let add_type_definition t ts tk = Hts.replace t.type_defs ts tk

let get_type_definition t ts = Hts.find t.type_defs ts
