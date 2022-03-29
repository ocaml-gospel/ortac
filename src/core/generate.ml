module W = Warnings
open Ppxlib
open Builder
open Translated
module F = Failure
module T = Translation
module M = Map.Make (String)

let setup name loc register_name next =
  [%expr
    let [%p pvar register_name] =
      Ortac_runtime.Errors.create [%e elocation loc] [%e estring name]
    in
    [%e next]]

let term (t : Translated.term) next =
  match t.translation with Error _ -> next | Ok c -> pexp_sequence c next

let terms terms next = List.fold_left (fun acc t -> term t acc) next terms

let check positive (c : Translated.check) next =
  match c.translations with
  | Error _ -> next
  | Ok (p, n) -> pexp_sequence (if positive then p else n) next

let checks positive checks next =
  List.fold_left (fun acc t -> check positive t acc) next checks

let invariants ~register_name pos instance (t : type_) next =
  List.fold_left
    (fun next (t : invariant) ->
      match t.translation with
      | Error _ -> next
      | Ok (name, _) ->
          pexp_sequence
            (eapply (evar name) [ register_name; evar pos; evar instance ])
            next)
    next t.invariants

let var_invariants ~register_name pos ignore_consumes (v : ocaml_var) next =
  if ignore_consumes && v.consumed then next
  else invariants ~register_name pos v.name v.type_ next

let vars_invariants ~register_name pos ignore_consumes vl next =
  List.fold_left
    (fun acc t -> var_invariants ~register_name pos ignore_consumes t acc)
    next vl

let group_xpost (v : Translated.value) =
  let register_name = evar v.register_name in
  let invariants = vars_invariants ~register_name "XPost" true v.arguments in
  let default_cases =
    [
      case ~guard:None
        ~lhs:[%pat? (Stack_overflow | Out_of_memory) as e]
        ~rhs:
          [%expr
            [%e checks true v.checks @@ invariants @@ F.report ~register_name];
            raise e];
      case ~guard:None
        ~lhs:[%pat? e]
        ~rhs:
          [%expr
            [%e F.unexpected_exn ~allowed_exn:[] ~exn:(evar "e") ~register_name];
            [%e checks true v.checks @@ invariants @@ F.report ~register_name];
            raise e];
    ]
  in
  let default_cases =
    let invalid_arg_case =
      case ~guard:None
        ~lhs:[%pat? Invalid_argument _ as e]
        ~rhs:
          [%expr
            [%e checks false v.checks @@ invariants @@ F.report ~register_name];
            raise e]
    in
    if v.checks = [] then default_cases else invalid_arg_case :: default_cases
  in
  let tbl = Hashtbl.create 0 in
  let rec aux keys = function
    | [] -> keys
    | { exn; args; translation = Ok translation } :: t ->
        Hashtbl.add tbl exn translation;
        aux (M.add exn args keys) t
    | _ :: t -> aux keys t
  in
  aux M.empty v.xpostconditions |> fun s ->
  M.fold
    (fun exn args acc ->
      let e = gen_symbol ~prefix:"__e_" () in
      let lhs =
        ppat_alias
          (ppat_construct (lident exn)
             (if args = 0 then None else Some ppat_any))
          (noloc e)
      in
      let matches =
        Hashtbl.find_all tbl exn |> List.map (pexp_match (evar e)) |> esequence
      in
      let rhs =
        esequence
          [
            matches;
            checks true v.checks @@ invariants @@ F.report ~register_name;
            eapply (evar "raise") [ evar e ];
          ]
      in
      case ~guard:None ~lhs ~rhs :: acc)
    s default_cases

let args f = List.map (fun a -> (a.label, f a.name))

let rets (returns : ocaml_var list) =
  match returns with
  | [] -> (eunit, punit)
  | [ x ] -> (evar x.name, pvar x.name)
  | ret ->
      List.fold_right
        (fun (x : ocaml_var) (e, p) -> (evar x.name :: e, pvar x.name :: p))
        ret ([], [])
      |> fun (e, p) -> (pexp_tuple e, ppat_tuple p)

let value (v : Translated.value) =
  let register_name = evar v.register_name in
  let report = pexp_sequence (F.report ~register_name) in
  let eargs = args evar v.arguments in
  let pargs = args pvar v.arguments in
  let eret, pret = rets v.returns in
  let call = pexp_apply (evar v.name) eargs in
  let try_call = pexp_try call (group_xpost v) in
  let body =
    setup v.name v.loc v.register_name
    @@ terms v.preconditions
    @@ vars_invariants ~register_name "Pre" false v.arguments
    @@ report
    @@ pexp_let Nonrecursive [ value_binding ~pat:pret ~expr:try_call ]
    @@ terms v.postconditions
    @@ checks true v.checks
    @@ vars_invariants ~register_name "Post" true v.arguments
    @@ vars_invariants ~register_name "Post" false v.returns
    @@ report
    @@ eret
  in
  [ [%stri let [%p pvar v.name] = [%e efun pargs body]] ]

let function_ (f : Translated.function_) =
  match f.definition with
  | Some { translation = Ok def; _ } ->
      let pat = pvar f.name in
      let pargs = args pvar f.arguments in
      let expr = efun pargs def in
      let rec_flag = if f.rec_ then Recursive else Nonrecursive in
      [ pstr_value rec_flag [ value_binding ~pat ~expr ] ]
  | _ -> []

let constant (c : Translated.constant) =
  let register_name = evar c.register_name in
  let report = pexp_sequence (F.report ~register_name) in
  let body =
    setup c.name c.loc c.register_name
    @@ terms c.checks
    @@ invariants ~register_name "Pre" c.name c.type_
    @@ report
    @@ evar c.name
  in
  [ [%stri let [%p pvar c.name] = [%e body]] ]

let type_ (t : Translated.type_) =
  List.filter_map
    (fun (i : invariant) -> Result.to_option i.translation |> Option.map snd)
    t.invariants

let axiom (a : Translated.axiom) =
  let register_name = evar a.register_name in
  let report = pexp_sequence (F.report ~register_name) in
  let body =
    setup a.name a.loc a.register_name @@ term a.definition @@ report @@ eunit
  in
  [ [%stri let () = [%e body]] ]

let repr ~driver : structure =
  let f i =
    let open Derive in
    match (expr i, eq i) with
    | _, None -> []
    | Base repr, Some eq ->
        let n = gen_symbol ~prefix:"__repr" () in
        [
          [%stri let [%p pvar n] = [%e repr]];
          [%stri let [%p pvar eq] = Repr.(unstage (equal [%e evar n]))];
        ]
    | _, _ -> []
  in
  Drv.map_reprs ~f driver |> List.flatten

let structure runtime driver : structure =
  (pmod_ident (lident (Drv.module_name driver)) |> include_infos |> pstr_include)
  :: pstr_module
       (module_binding
          ~name:{ txt = Some "Ortac_runtime"; loc }
          ~expr:(pmod_ident (lident runtime)))
  :: repr ~driver
  @ (Drv.map_translation driver ~f:(function
       | Translated.Value v -> value v
       | Translated.Function f -> function_ f
       | Translated.Predicate f -> function_ f
       | Translated.Constant c -> constant c
       | Translated.Type t -> type_ t
       | Translated.Axiom a -> axiom a)
    |> List.flatten)
