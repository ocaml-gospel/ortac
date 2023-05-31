module Cfg = Config
open Ir
open Ppxlib
open Ortac_core.Builder

let ty_default = Ptyp_constr (noloc (Lident "char"), [])

let show_attribute : attribute =
  {
    attr_name = noloc "deriving";
    attr_payload = PStr [ [%stri show { with_path = false }] ];
    attr_loc = Location.none;
  }

let subst_core_type inst ty =
  let rec aux ty =
    {
      ty with
      ptyp_desc =
        (match ty.ptyp_desc with
        | Ptyp_any -> Ptyp_any
        | Ptyp_var x ->
            Option.fold ~none:ty_default
              ~some:(fun x -> x.ptyp_desc)
              (List.assoc_opt x inst)
        | Ptyp_arrow (x, l, r) ->
            let l = aux l and r = aux r in
            Ptyp_arrow (x, l, r)
        | Ptyp_tuple elems ->
            let elems = List.map aux elems in
            Ptyp_tuple elems
        | Ptyp_constr (c, args) ->
            let args = List.map aux args in
            Ptyp_constr (c, args)
        | Ptyp_object (_, _)
        | Ptyp_class (_, _)
        | Ptyp_alias (_, _)
        | Ptyp_variant (_, _, _)
        | Ptyp_poly (_, _)
        | Ptyp_package _ | Ptyp_extension _ ->
            failwith "Case should not happen in `subst'");
    }
  in
  aux ty

let subst_term ~gos_t ~old_t ~new_t term =
  let exception ImpossibleSubst of (Gospel.Tterm.term * [ `New | `Old ]) in
  let rec aux cur_t term =
    let open Gospel.Tterm in
    let next = aux cur_t in
    match term.t_node with
    | Tconst _ -> term
    | Tvar { vs_name; vs_ty } when vs_name = gos_t -> (
        match cur_t with
        | Some cur_t -> { term with t_node = Tvar { vs_name = cur_t; vs_ty } }
        | None ->
            raise (ImpossibleSubst (term, if cur_t = new_t then `New else `Old))
        )
    | Tvar _ -> term
    | Tapp (ls, terms) -> { term with t_node = Tapp (ls, List.map next terms) }
    | Tfield (t, ls) -> { term with t_node = Tfield (next t, ls) }
    | Tif (cnd, thn, els) ->
        { term with t_node = Tif (next cnd, next thn, next els) }
    | Tlet (vs, t1, t2) -> { term with t_node = Tlet (vs, next t1, next t2) }
    | Tcase (t, brchs) ->
        {
          term with
          t_node =
            Tcase
              ( next t,
                List.map
                  (fun (p, ot, t) -> (p, Option.map next ot, next t))
                  brchs );
        }
    | Tquant (q, vs, t) -> { term with t_node = Tquant (q, vs, next t) }
    | Tbinop (o, l, r) -> { term with t_node = Tbinop (o, next l, next r) }
    | Tnot t -> { term with t_node = Tnot (next t) }
    | Told t -> aux old_t t
    | Ttrue -> term
    | Tfalse -> term
  in
  let open Reserr in
  try ok (aux new_t term)
  with ImpossibleSubst (t, b) ->
    error
      ( Impossible_term_substitution
          (Fmt.str "%a" Gospel.Tterm_printer.print_term t, b),
        t.t_loc )

let next_state_case config state_ident value =
  let str_of_ident = Fmt.str "%a" Gospel.Identifier.Ident.pp in
  let state_var = str_of_ident state_ident |> evar in
  let lhs =
    let pat_args = function
      | None -> punit
      | Some x -> ppat_var (noloc (str_of_ident x))
    in
    let args =
      match value.args with
      | [] -> None
      | [ x ] -> Some (pat_args x)
      | xs -> List.map pat_args xs |> ppat_tuple |> Option.some
    in
    let name = String.capitalize_ascii (str_of_ident value.id) |> lident in
    ppat_construct name args
  in
  let open Reserr in
  let term t =
    let open Ortac_core.Ocaml_of_gospel in
    try term ~context:config.Cfg.context t |> ok with W.Error e -> error e
  in
  let* idx, rhs =
    (* substitute state variable when under `old` operator and translate description into ocaml *)
    let descriptions =
      List.filter_map
        (fun (i, { model; description }) ->
          subst_term ~gos_t:value.sut_var ~old_t:(Some state_ident) ~new_t:None
            description
          >>= term
          |> to_option
          |> Option.map (fun description -> (i, model, description)))
        value.next_state.formulae
    in
    (* choose one and only one description per modified model *)
    let pick id =
      List.find_opt
        (fun (_, m, _) -> Gospel.Identifier.Ident.equal id m)
        descriptions
    in
    let* descriptions =
      map
        (fun id ->
          of_option
            ~default:
              (Ensures_not_found_for_next_state (str_of_ident id), id.id_loc)
            (pick id))
        value.next_state.modifies
    in
    let idx = List.map (fun (i, _, _) -> i) descriptions in
    match
      List.map (fun (_, m, e) -> (lident (str_of_ident m), e)) descriptions
    with
    | [] -> ok (idx, state_var)
    | fields ->
        (idx, pexp_record fields (Some (evar (str_of_ident state_ident)))) |> ok
  in
  (idx, case ~lhs ~guard:None ~rhs) |> ok

let next_state config ir =
  let cmd_name = gen_symbol ~prefix:"cmd" () in
  let state_name = gen_symbol ~prefix:"state" () in
  let state_ident = Gospel.Tast.Ident.create ~loc:Location.none state_name in
  let open Reserr in
  let* idx_cases =
    map
      (fun v ->
        let* i, c = next_state_case config state_ident v in
        ok ((v.id, i), c))
      ir.values
  in
  let idx, cases = List.split idx_cases in
  let body = pexp_match (evar cmd_name) cases in
  let pat = pvar "next_state" in
  let expr =
    efun [ (Nolabel, pvar cmd_name); (Nolabel, pvar state_name) ] body
  in
  (idx, pstr_value Nonrecursive [ value_binding ~pat ~expr ]) |> ok

let cmd_constructor config value =
  let rec aux ty : Ppxlib.core_type list =
    match ty.ptyp_desc with
    | Ptyp_arrow (_, l, r) ->
        if Cfg.is_sut config l then aux r
        else
          let x = subst_core_type value.inst l and xs = aux r in
          x :: xs
    | _ -> []
  in
  let name =
    String.capitalize_ascii value.id.Gospel.Tast.Ident.id_str |> noloc
  in
  let args = aux value.ty in
  constructor_declaration ~name ~args:(Pcstr_tuple args) ~res:None

let state_type ir =
  let lds =
    List.map
      (fun (id, ty) ->
        label_declaration
          ~name:(Fmt.str "%a" Gospel.Tast.Ident.pp id |> noloc)
          ~mutable_:Immutable ~type_:ty)
      ir.state
  in
  let kind = Ptype_record lds in
  let td =
    type_declaration ~name:(noloc "state") ~params:[] ~cstrs:[] ~kind
      ~private_:Public ~manifest:None
  in
  pstr_type Nonrecursive [ td ]

let cmd_type config ir =
  let constructors = List.map (cmd_constructor config) ir.values in
  let td =
    type_declaration ~name:(noloc "cmd") ~params:[] ~cstrs:[]
      ~kind:(Ptype_variant constructors) ~private_:Public ~manifest:None
  in
  pstr_type Nonrecursive [ { td with ptype_attributes = [ show_attribute ] } ]

let stm config ir =
  let cmd = cmd_type config ir in
  let state = state_type ir in
  let open Reserr in
  let* _idx, next_state = next_state config ir in
  ok [ cmd; state; next_state ]
