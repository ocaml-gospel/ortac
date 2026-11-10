open Gospel_typecheckerlibs
open Namespace

open struct
  let mk_qualid xs =
    let open Parse_uast in
    let pid x = Preid.create ~loc:Location.none x in
    let qid x = Qid (pid x) and qdot q x = Qdot (q, pid x) in
    let rec aux = function
      | [] -> invalid_arg "mk_qualid"
      | [ x ] -> qid x
      | x :: xs -> qdot (aux xs) x
    in
    aux @@ List.rev xs

  let find_module str namespace =
    let _, m = access_mod namespace @@ mk_qualid str in
    m

  let get_fun_ident namespace str = get_fun_ident namespace @@ mk_qualid str

  let get_gospel_type_ident namespace str =
    get_gospel_type_ident namespace @@ mk_qualid str
end

module L = Map.Make (struct
  type t = Ident.t

  let equal = Ident.equal
  let compare x y = compare x.Ident.id_tag y.Ident.id_tag
end)

module T = Map.Make (struct
  type t = Ident.t

  let equal = Ident.equal
  let compare x y = compare x.Ident.id_tag y.Ident.id_tag
end)

type t = {
  module_name : string;
  stdlib : string L.t;
  tystdlib : string T.t;
  env : mod_defs;
  functions : string L.t;
}

let get_env get ns path =
  try get ns path
  with Not_found ->
    Fmt.(
      failwith "Internal error: path `%a' was not found"
        (list ~sep:(any ".") string)
        path)

let translate_stdlib ls t = L.find_opt ls t.stdlib
let translate_tystdlib ts t = T.find_opt ts t.tystdlib
let add_function ls i t = { t with functions = L.add ls i t.functions }
let find_function ls t = L.find ls t.functions
let is_function ls t = L.mem ls t.functions
let get_ls_env = get_fun_ident
let get_ls t = get_fun_ident t.env
let get_ts t = get_gospel_type_ident t.env

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
    let v = Env.fold (f path) (proj ns) v in
    Env.fold
      (fun s mod_info -> aux (path @ [ s ]) mod_info.mdefs)
      (get_mod_env ns) v
  in
  aux path ns v

let init module_name env =
  let gostd = find_module [ "Gospelstdlib" ] env in
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
    fold_namespace get_fun_env
      (process_gostd_entry @@ fun info -> L.add info.fid)
      [ "Gospelstdlib" ] gostd stdlib
  in
  let tystdlib =
    List.fold_left
      (fun acc (path, ocaml) ->
        let ts = get_gospel_type_ident env path in
        T.add ts ocaml acc)
      T.empty tybuiltins
  in
  let tystdlib =
    fold_namespace get_type_env
      (process_gostd_entry @@ fun info -> T.add info.tid)
      [ "Gospelstdlib" ] gostd tystdlib
  in
  { module_name; stdlib; tystdlib; env; functions = L.empty }

let module_name t = t.module_name
