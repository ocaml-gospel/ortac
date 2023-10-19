open Gospel.Tmodule
module L = Map.Make (Gospel.Symbols.LS)
module T = Map.Make (Gospel.Ttypes.Ts)

type t = {
  module_name : string;
  stdlib : string L.t;
  tystdlib : string T.t;
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
let translate_tystdlib ts t = T.find_opt ts t.tystdlib
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

let tybuiltins =
  [
    ([ "unit" ], "unit");
    ([ "string" ], "string");
    ([ "char" ], "char");
    ([ "float" ], "float");
    ([ "bool" ], "bool");
    ([ "int" ], "int");
    ([ "option" ], "option");
    ([ "list" ], "list");
    ([ "integer" ], "Ortac_runtime.integer");
  ]

(** [unsupported_stdlib] contains all the entries of the Gospel Stdlib that
    cannot be translated *)
let unsupported_stdlib =
  let t = Hashtbl.create 1 in
  List.iter
    (fun f -> Hashtbl.add t ("Gospelstdlib" :: f) ())
    [ [ "Order"; "is_pre_order" ] ];
  t

(** Map a name from the Gospel parser to an OCaml name when possible

    The strategy is as follows, always dropping the "*fix " prefix of the
    operator name:

    - infix operators are kept as is
    - prefix operators are prefixed with ~
    - mixfix operators are prefixed with __mix_ and encoded to ascii
    - other names are kept as is

    TODO? maybe, some filtering should be performed also on other names, such as
    names containing "#" and return [None] on those *)
let process_name name =
  let convert_mix_symbol = function
    | '-' -> 'm' (* minus *)
    | '.' -> 'd' (* dot *)
    | '>' -> 'g' (* greater *)
    | '[' -> 'B' (* bracket (open) *)
    | ']' -> 'b' (* bracket (close) *)
    | '_' -> 'u' (* underscore *)
    | '{' -> 'C' (* curly (open) *)
    | '}' -> 'c' (* curly (close) *)
    | c -> failwith (Printf.sprintf "Cannot convert mixfix symbol '%c'" c)
    (* this should not happen: all the symbols used in the Stdlib
       should be covered *)
  in

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
  else if starts p_mix then
    Some ("__mix_" ^ String.map convert_mix_symbol (drop p_mix))
  else Some name

let fold_namespace proj f path ns v =
  let rec aux path ns v =
    let v = Gospel.Tmodule.Mstr.fold (f path) (proj ns) v in
    Gospel.Tmodule.Mstr.fold (fun s -> aux (path @ [ s ])) ns.ns_ns v
  in
  aux path ns v

let init module_name env =
  let gostd = Gospel.Tmodule.Mstr.find "Gospelstdlib" env.ns_ns in
  let process_gostd_entry add path name sym lib =
    match process_name name with
    | None -> lib
    | Some name ->
        let fullpath = path @ [ name ] in
        if Hashtbl.mem unsupported_stdlib fullpath then lib
        else
          let fullpath = "Ortac_runtime" :: fullpath in
          add sym (String.concat "." fullpath) lib
  in
  let stdlib =
    List.fold_left
      (fun acc (path, ocaml) ->
        let ls = get_ls_env env path in
        L.add ls ocaml acc)
      L.empty builtins
  in
  let stdlib =
    fold_namespace
      (fun ns -> ns.ns_ls)
      (process_gostd_entry L.add)
      [ "Gospelstdlib" ] gostd stdlib
  in
  let tystdlib =
    List.fold_left
      (fun acc (path, ocaml) ->
        let ts = get_env ns_find_ts env path in
        T.add ts ocaml acc)
      T.empty tybuiltins
  in
  let tystdlib =
    fold_namespace
      (fun ns -> ns.ns_ts)
      (process_gostd_entry T.add)
      [ "Gospelstdlib" ] gostd tystdlib
  in
  { module_name; stdlib; tystdlib; env; functions = L.empty }

let module_name t = t.module_name
