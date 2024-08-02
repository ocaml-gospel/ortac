open Gospel
open Tast
module Cfg = Config
module Ident = Identifier.Ident

let constant_test vd =
  let open Reserr in
  match vd.vd_args with
  | [] ->
      (Constant_value (Fmt.str "%a" Ident.pp vd.vd_name), vd.vd_loc) |> error
  | _ -> ok ()

let no_functional_arg_or_big_tuple vd =
  let open Reserr in
  let open Ppxlib in
  let rec contains_arrow ty =
    match ty.ptyp_desc with
    | Ptyp_arrow (_, _, _) ->
        error (Functional_argument vd.vd_name.id_str, ty.ptyp_loc)
    | Ptyp_tuple xs when List.length xs > 9 ->
        error (Tuple_arity vd.vd_name.id_str, ty.ptyp_loc)
    | Ptyp_tuple xs | Ptyp_constr (_, xs) -> traverse_ contains_arrow xs
    | _ -> ok ()
  in
  let rec aux ty =
    match ty.ptyp_desc with
    | Ptyp_arrow (_, l, r) ->
        let* _ = contains_arrow l in
        aux r
    | _ -> ok ()
  in
  aux vd.vd_type

let is_a_function ty =
  let open Ppxlib in
  match ty.ptyp_desc with Ptyp_arrow (_, _, _) -> true | _ -> false

let unify case sut_ty ty =
  let open Ppxlib in
  let open Reserr in
  let fail () =
    let sut_ty = Fmt.to_to_string Pprintast.core_type sut_ty in
    match case with
    | `Value name -> error (Incompatible_type (name, sut_ty), ty.ptyp_loc)
    | `Type ty -> error (Incompatible_sut sut_ty, ty.td_loc)
  in
  let add_if_needed a x i =
    match List.assoc_opt a i with
    | None -> ok ((a, x) :: i)
    | Some y when x.ptyp_desc = y.ptyp_desc -> ok i
    | _ -> fail ()
  in
  let rec aux i = function
    | [], [] -> ok i
    | x :: xs, y :: ys ->
        let* i =
          match (x.ptyp_desc, y.ptyp_desc) with
          | _, Ptyp_any -> ok i
          | _, Ptyp_var a -> add_if_needed a x i
          | Ptyp_tuple xs, Ptyp_tuple ys -> aux i (xs, ys)
          | Ptyp_constr (c, xs), Ptyp_constr (d, ys) when c.txt = d.txt ->
              aux i (xs, ys)
          | _ -> fail ()
        in
        aux i (xs, ys)
    | _, _ -> fail ()
  in
  match (sut_ty.ptyp_desc, ty.ptyp_desc) with
  | Ptyp_constr (t, args_sut), Ptyp_constr (t', args_ty) when t.txt = t'.txt ->
      aux [] (args_sut, args_ty)
  | _ -> failwith "Case should not happen in `unify'"

let ty_var_substitution config (vd : val_description) =
  let value_name, value_type = (vd.vd_name.id_str, vd.vd_type) in
  assert (is_a_function value_type);
  let open Ppxlib in
  let open Reserr in
  let rec check pos ty =
    match ty.ptyp_desc with
    | Ptyp_constr (_, args) -> (
        match List.find_opt (Cfg.is_sut config) args with
        | Some _ -> (
            match pos with
            | `Left -> error (Sut_as_type_inst value_name, ty.ptyp_loc)
            | `Right -> error (Returning_nested_sut value_name, ty.ptyp_loc))
        | None -> traverse_ (check pos) args)
    | Ptyp_tuple elems -> (
        match List.find_opt (Cfg.is_sut config) elems with
        | Some _ -> (
            match pos with
            | `Left -> error (Sut_in_tuple value_name, ty.ptyp_loc)
            | `Right -> error (Returning_nested_sut value_name, ty.ptyp_loc))
        | None -> traverse_ (check pos) elems)
    | _ -> ok ()
  in
  let rec aux insts ty =
    match ty.ptyp_desc with
    | Ptyp_any | Ptyp_var _ -> Reserr.ok insts
    | Ptyp_arrow (_, l, r) ->
        let* () = check `Left l in
        if Cfg.is_sut config l then
          let open Reserr in
          let* x = unify (`Value value_name) config.sut_core_type l in
          aux (insts @ x) r
        else aux insts r
    | Ptyp_tuple _ ->
        let* () = check `Right ty in
        Reserr.ok insts
    | Ptyp_constr _ when Cfg.is_sut config ty ->
        let* () = check `Right ty in
        let* x = unify (`Value value_name) config.sut_core_type ty in
        insts @ x |> ok
    | Ptyp_constr (_, _) ->
        let* () = check `Right ty in
        Reserr.ok insts
    (* not supported *)
    | Ptyp_object (_, _)
    | Ptyp_class (_, _)
    | Ptyp_alias (_, _)
    | Ptyp_variant (_, _, _)
    | Ptyp_poly (_, _)
    | Ptyp_package _ | Ptyp_extension _ ->
        failwith "not supported"
  in
  aux [] value_type

let split_args config vd args =
  let open Ppxlib in
  let open Reserr in
  let module Ident = Identifier.Ident in
  let rec aux suts acc ty args =
    match (ty.ptyp_desc, args) with
    | _, Lghost vs :: _ ->
        error (Ghost_values (vd.vd_name.id_str, `Arg), vs.vs_name.id_loc)
    | Ptyp_arrow (_, l, r), Lnone vs :: xs
    | Ptyp_arrow (_, l, r), Loptional vs :: xs
    | Ptyp_arrow (_, l, r), Lnamed vs :: xs ->
        if Cfg.is_sut config l then aux (vs.vs_name :: suts) acc r xs
        else aux suts ((l, Some vs.vs_name) :: acc) r xs
    | Ptyp_arrow (_, l, r), Lunit :: xs -> aux suts ((l, None) :: acc) r xs
    | _, [] -> ok (List.rev suts, List.rev acc)
    | _, _ -> failwith "shouldn't happen (too few parameters)"
  in
  aux [] [] vd.vd_type args

let get_state_description_with_index is_t state spec =
  let open Tterm in
  let rec pred i t =
    match t.t_node with
    | Tapp (ls, [ { t_node = Tfield ({ t_node = Tvar vs; _ }, m); _ }; right ])
      when Symbols.(ls_equal ps_equ ls) && is_t vs ->
        if List.exists (fun (id, _) -> Ident.equal id m.ls_name) state then
          Some (i, Ir.{ model = m.ls_name; description = right })
        else None
    | Tbinop ((Tand | Tand_asym), l, r) -> (
        match pred i l with None -> pred i r | o -> o)
    | _ -> None
  in
  List.mapi pred spec.sp_post |> List.filter_map Fun.id

let next_states suts state spec =
  let next_state sut =
    let open Tterm in
    let is_t vs = Ident.equal sut vs.Symbols.vs_name in
    let formulae = get_state_description_with_index is_t state spec in
    let check_modify = function
      | { t_node = Tvar vs; _ } as t when is_t vs ->
          List.map (fun (m, _) -> (m, t.t_loc)) state
      | { t_node = Tfield ({ t_node = Tvar vs; _ }, m); _ } as t when is_t vs ->
          [ (m.ls_name, t.t_loc) ]
      | _ -> []
    in
    let modifies = List.concat_map check_modify spec.sp_wr in
    let modifies =
      List.sort_uniq (fun (i, _) (i', _) -> Ident.compare i i') modifies
    in
    (sut, Ir.{ formulae; modifies })
  in
  List.map next_state suts

let ret_next_state config state vd =
  let open Reserr in
  let open Ppxlib in
  let open Gospel.Symbols in
  if Cfg.does_return_sut config vd.vd_type then
    let spec = Option.get vd.vd_spec in
    let returned_sut =
      match spec.sp_ret with
      | [ Lnone vs ] -> vs.vs_name
      | _ -> failwith "should not happen"
    in
    let is_t vs = Ident.equal returned_sut vs.vs_name in
    let formulae = get_state_description_with_index is_t state spec in
    let modifies = List.map (fun (field, _) -> (field, Location.none)) state in
    (* Check that there is at least one equation for each model field *)
    let* () =
      match
        List.filter
          (fun (id, _) ->
            not
              (List.exists
                 (fun (_, Ir.{ model; _ }) -> Ident.equal model id)
                 formulae))
          state
      with
      | [] -> ok ()
      | missing_models ->
          let missing_models =
            List.map (fun (id, _) -> id.Ident.id_str) missing_models
          in
          error
            ( Ensures_not_found_for_ret_sut
                (vd.vd_name.Ident.id_str, missing_models),
              spec.sp_loc )
    in
    Some (returned_sut, Ir.{ formulae; modifies }) |> ok
  else None |> ok

(* returns the list of terms [t] such that [ret = t] appears in the [ensures] of
   the [spec] *)
let returned_value_description spec ret =
  let open Tterm in
  let is_ret vs =
    let open Symbols in
    Ident.equal ret vs.vs_name
  in
  let rec pred t =
    match t.t_node with
    (* match [ret = term] and return [term] *)
    | Tapp (ls, [ { t_node = Tvar vs; _ }; right ])
      when Symbols.(ls_equal ps_equ ls) && is_ret vs ->
        [ Ir.term_val spec right ]
    (* Gospel automatically inserts a cast from int to integer when needed *)
    | Tapp
        (ls, [ { t_node = Tapp (func, [ { t_node = Tvar vs; _ } ]); _ }; right ])
      when String.equal func.ls_name.id_str "integer_of_int"
           && Symbols.(ls_equal ps_equ ls)
           && is_ret vs ->
        [ Ir.term_val spec right ]
    | Tbinop ((Tand | Tand_asym), l, r) -> pred l @ pred r
    | _ -> []
  in
  List.concat_map pred spec.sp_post

let postcond spec =
  let normal = List.mapi (fun i x -> (i, Ir.term_val spec x)) spec.sp_post
  and exceptional =
    List.concat_map
      (fun (x, l) ->
        match l with
        | [] ->
            [
              ( x,
                None,
                Ir.
                  {
                    term = Tterm_helper.t_true Ppxlib.Location.none;
                    text = "true";
                  } );
            ]
        | _ ->
            List.rev_map
              (fun (p, t) ->
                let open Tterm in
                match p.p_node with
                | Papp (ls, []) when Symbols.(ls_equal ls (fs_tuple 0)) ->
                    (x, None, Ir.term_val spec t)
                | _ -> (x, Some p, Ir.term_val spec t))
              l)
      spec.sp_xpost
  in
  Ir.
    { normal; exceptional; checks = List.map (Ir.term_val spec) spec.sp_checks }

let val_desc config state vd =
  let open Reserr in
  let* () = constant_test vd and* () = no_functional_arg_or_big_tuple vd in
  let* inst = ty_var_substitution config vd
  and* spec =
    of_option ~default:(No_spec vd.vd_name.id_str, vd.vd_loc) vd.vd_spec
  in
  let* suts, args = split_args config vd spec.sp_args
  and* ret =
    let p = function
      | Lnone vs -> ok vs.vs_name
      | Lghost vs ->
          error (Ghost_values (vd.vd_name.id_str, `Ret), vs.vs_name.id_loc)
      | _ ->
          failwith
            "shouldn't happen (only non labelled and ghost value are returned)"
    in
    traverse p spec.sp_ret
  in
  let ret_values = List.map (returned_value_description spec) ret in
  let next_states = next_states suts state spec in
  let* ret_state = ret_next_state config state vd in
  let next_states =
    match ret_state with
    | Some ret_state -> ret_state :: next_states
    | None -> next_states
  in
  let postcond = postcond spec in
  Ir.value vd.vd_name vd.vd_type inst suts args ret ret_values next_states
    spec.sp_pre postcond
  |> ok

let sig_item config state s =
  match s.sig_desc with
  | Sig_val (vd, Nonghost) -> Some (val_desc config state vd)
  | _ -> None

let state_and_invariants config sigs =
  let sut_name = Cfg.get_sut_type_name_str config in
  let is_sut_decl s =
    let p t = t.td_ts.ts_ident.id_str = sut_name in
    match s.sig_desc with
    | Sig_type (_, ts, _) -> List.find_opt p ts
    | _ -> None
  in
  let open Reserr in
  let* ty =
    List.filter_map is_sut_decl sigs
    |> Fun.flip List.nth_opt 0
    |> of_option ~default:(No_sut_type sut_name, Ppxlib.Location.none)
  in
  let open Ortac_core in
  let* subst =
    Fun.flip List.assoc_opt
    <$> (Ocaml_of_gospel.core_type_of_tysymbol ~context:config.context ty.td_ts
        |> unify (`Type ty) config.sut_core_type)
  and* spec =
    of_option ~default:(Sut_type_not_specified sut_name, ty.td_loc) ty.td_spec
  in
  let process_model (ls, _) =
    let open Symbols in
    ( ls.ls_name,
      Option.get ls.ls_value
      |> Ocaml_of_gospel.core_type_of_ty_with_subst ~context:config.context
           subst )
  in
  let* state =
    match spec.ty_fields with
    | [] -> error (No_models sut_name, spec.ty_loc)
    | xs -> List.map process_model xs |> ok
  in
  let invariants =
    Option.map
      (fun (vs, xs) -> (vs.Symbols.vs_name, List.map (Ir.term_type spec) xs))
      spec.ty_invariants
  in
  ok (state, invariants)

let init_state config state sigs =
  let open Cfg in
  let open Reserr in
  let open Ppxlib in
  let* fct, args =
    match config.init_sut with
    | {
     pexp_desc =
       Pexp_apply ({ pexp_desc = Pexp_ident { txt = lid; _ }; _ }, args);
     _;
    } ->
        ok (lid, List.map snd args)
    | expr ->
        error
          ( Impossible_init_state_generation
              (Not_a_function_call config.init_sut_txt),
            expr.pexp_loc )
  in
  let* fct_str =
    match fct with
    | Lident s -> ok s
    | Ldot (_, _) | Lapply (_, _) ->
        let name =
          match config.init_sut with
          | { pexp_desc = Pexp_apply (id, _); _ } -> id
          | _ -> assert false
        in
        error
          ( Impossible_init_state_generation
              (Qualified_name Fmt.(str "%a" Pprintast.expression name)),
            Location.none )
  in
  let is_init_declaration = function
    | { sig_desc = Sig_val (v, _); _ }
      when Fmt.str "%a" Gospel.Identifier.Ident.pp v.vd_name = fct_str ->
        Some v
    | _ -> None
  in
  let* value =
    List.rev sigs
    (* we want the last definition *)
    |> List.find_map is_init_declaration
    |> of_option ~default:(No_init_function fct_str, Location.none)
  in
  let* spec =
    of_option
      ~default:
        ( Impossible_init_state_generation
            (No_specification value.Tast.vd_name.id_str),
          value.vd_loc )
      value.vd_spec
  in
  let* arguments =
    let is_not_ghost = function Lghost _ -> false | _ -> true in
    let sp_args = List.filter is_not_ghost spec.sp_args in
    try List.combine sp_args args |> ok
    with Invalid_argument _ ->
      error
        ( Impossible_init_state_generation
            (Mismatch_number_of_arguments config.init_sut_txt),
          config.init_sut.pexp_loc )
  in
  let open Gospel.Symbols in
  let rec return_type ty =
    match ty.ptyp_desc with Ptyp_arrow (_, _, r) -> return_type r | _ -> ty
  in
  let ret_sut = Cfg.is_sut config (return_type value.vd_type) in
  let* returned_sut =
    List.find_map
      (function Lnone vs when ret_sut -> Some vs.vs_name | _ -> None)
      spec.sp_ret
    |> of_option
         ~default:
           ( Impossible_init_state_generation
               (Not_returning_sut
                  (Fmt.str "%a" Gospel.Identifier.Ident.pp value.Tast.vd_name)),
             value.vd_loc )
  in
  let is_t vs = Ident.equal returned_sut vs.vs_name in
  let descriptions =
    get_state_description_with_index is_t state spec |> List.map snd
  in
  let* () =
    match
      (* at least one description per model in state, choice is made when translating *)
      List.filter
        (fun (id, _) ->
          not
            (List.exists
               (fun Ir.{ model; _ } -> Ident.equal model id)
               descriptions))
        state
    with
    | [] -> ok ()
    | missing_models ->
        let missing_models =
          List.map (fun (id, _) -> id.Ident.id_str) missing_models
        in
        error
          ( Impossible_init_state_generation
              (No_appropriate_specifications (fct_str, missing_models)),
            spec.sp_loc )
  in
  Ir.{ arguments; returned_sut; descriptions } |> ok

let signature config state sigs =
  List.filter_map (sig_item config state) sigs |> Reserr.promote

let ghost_functions =
  let open Tast in
  List.filter_map (fun s ->
      match s.sig_desc with
      | Sig_function f when Option.is_some f.fun_def -> Some f
      | _ -> None)

let ghost_types =
  let open Tast in
  List.filter_map (fun s ->
      match s.sig_desc with
      | Sig_type (rec_flag, type_decls, Ghost) -> Some (rec_flag, type_decls)
      | _ -> None)

let run sigs config =
  let open Reserr in
  let open Ir in
  let* state, invariants = state_and_invariants config sigs in
  let* init_state = init_state config state sigs in
  let ghost_functions = ghost_functions sigs in
  let ghost_types = ghost_types sigs in
  let* values = signature config state sigs in
  ok { state; invariants; init_state; ghost_functions; ghost_types; values }
