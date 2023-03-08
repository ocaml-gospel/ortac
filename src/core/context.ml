open Gospel.Tmodule
module L = Map.Make (Gospel.Symbols.LS)

type t = {
  module_name : string;
  stdlib : string L.t;
  env : namespace;
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
let add_function ls i t = { t with functions = L.add ls i t.functions }
let find_function ls t = L.find ls t.functions
let is_function ls t = L.mem ls t.functions
let get_ls t = get_ls_env t.env
let get_ts t = get_env ns_find_ts t.env

let builtins =
  [
    ([ "None" ], "None");
    ([ "Some" ], "Some");
    ([ "[]" ], "[]");
    ([ "infix ::" ], "(::)");
    ([ "infix =" ], "(=)");
  ]

(** Map a name from the Gospel parser to an OCaml name when possible

    The strategy is as follows, always dropping the "*fix " prefix of the
    operator name:

    - infix operators are kept as is
    - prefix operators are prefixed with ~
    - mixfix operators are dropped
    - other names are kept as is

    TODO? maybe, some filtering should be performed also on other names, such as
    names containing "#" *)
let process_name name =
  let lname = String.length name in
  let drop prefix =
    let lp = String.length prefix in
    String.sub name lp (lname - lp)
  in
  let starts prefix = String.starts_with ~prefix name in
  let p_pre = "prefix " and p_in = "infix " and p_mix = "mixfix " in

  (* Here we could wrap operators in parentheses but as we are just building a
     string that will re-parsed soon enough into a proper identifier anyway, the
     shorter the better *)
  if starts p_pre then Some ("~" ^ drop p_pre)
  else if starts p_in then Some (drop p_in)
  else if starts p_mix then None
  else Some name

let fold_namespace f path ns v =
  let rec aux path ns v =
    let v = Gospel.Tmodule.Mstr.fold (f path) ns.ns_ls v in
    Gospel.Tmodule.Mstr.fold (fun s -> aux (path @ [ s ])) ns.ns_ns v
  in
  aux path ns v

let init module_name env =
  let gostd = Gospel.Tmodule.Mstr.find "Gospelstdlib" env.ns_ns in
  let process_gostd_entry path name ls lib =
    match process_name name with
    | None -> lib
    | Some name ->
        let fullpath = "Ortac_runtime" :: (path @ [ name ]) in
        L.add ls (String.concat "." fullpath) lib
  in
  let stdlib =
    List.fold_left
      (fun acc (path, ocaml) ->
        let ls = get_ls_env env path in
        L.add ls ocaml acc)
      L.empty builtins
  in
  let stdlib =
    fold_namespace process_gostd_entry [ "Gospelstdlib" ] gostd stdlib
  in
  { module_name; stdlib; env; functions = L.empty }

let module_name t = t.module_name
