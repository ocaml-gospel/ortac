module W = Warnings
open Ppxlib
open Gospel_typecheckerlibs
open Builder

let str_of_ident = Fmt.str "%a" Ident.pp
let str_of_qualid = Fmt.str "%a" Tast_printer.qualid
let id_of_qualid = function Id_uast.Qid id | Id_uast.Qdot (_, id) -> id

module M = struct
  let return a = (a, [])
  let put a = ((), [ a ])
  let value = fst

  let ( let* ) (x, xs) f =
    let y, ys = f x in
    (y, xs @ ys)

  let ( and* ) (x, xs) (y, ys) = ((x, y), xs @ ys)

  let sequence =
    let rec aux = function
      | [] -> return []
      | x :: xs ->
          let* y = x and* ys = aux xs in
          return (y :: ys)
    in
    aux

  let map f xs = List.map f xs |> sequence
  let some a = Some a |> return
end

type binop = Implies | Iff | And | And_asym | Or | Or_asym

let binop_of_str = function
  | "->" -> Implies
  | "<->" -> Iff
  | "/\\" -> And
  | "&&" -> And_asym
  | "\\/" -> Or
  | "||" -> Or_asym
  | _ -> raise Exit

let rec pattern_ p =
  let open Tast in
  let open M in
  match p.pat_desc with
  | Pwild -> return ppat_any
  | Pid v -> pvar (str_of_ident v.ts_id) |> return
  | Ptuple pl ->
      let* pattern_s = map pattern_ pl in
      ppat_tuple pattern_s |> return
  | Pcast (p, _) -> pattern_ p

let pattern p = pattern_ p |> M.value

type bound = Inf of expression | Sup of expression

let is_stdlib context q =
  Option.is_none @@ Context.translate_stdlib (id_of_qualid q) context

let is_conjunction context =
  let open Tast in
  function
  | { t_node = Tvar q; _ } when is_stdlib context q ->
      let str = str_of_ident @@ id_of_qualid q in
      String.equal "/\\" str || String.equal "&&" str
  | _ -> false

let is_implication context =
  let open Tast in
  function
  | { t_node = Tvar q; _ } when is_stdlib context q ->
      let str = str_of_ident @@ id_of_qualid q in
      String.equal "\\/" str || String.equal "||" str
  | _ -> false

let rec bounds ~context ~loc var (t1 : Tast.term) (t2 : Tast.term) =
  let unsupported () =
    raise W.(Error (Unsupported "ill formed quantification", loc))
  in
  let open Tast in
  (* [comb] extracts a bound from an the operator [f] and expression [e].
   [right] indicates if [e] is on the right side of the operator. *)
  let comb ~right f e =
    if Option.is_none @@ Context.translate_stdlib (id_of_qualid f) context then
      unsupported ();
    match str_of_ident @@ id_of_qualid f with
    | ">=" -> if right then Inf e else Sup e
    | "<=" -> if right then Sup e else Inf e
    | "<" -> if right then Sup (epred e) else Inf (esucc e)
    | ">" -> if right then Inf (esucc e) else Sup (epred e)
    | _ -> unsupported ()
  in
  let bound = function
    | Tast.Tapply
        ( {
            t_node = Tast.Tapply ({ t_node = Tvar f; _ }, { t_node = Tvar q; _ });
          },
          t )
      when Ident.equal (id_of_qualid q) var.ts_id ->
        comb ~right:true f (term ~context t)
    | Tast.Tapply
        ( { t_node = Tast.Tapply ({ t_node = Tvar f; _ }, t) },
          { t_node = Tvar q; _ } )
      when Ident.equal (id_of_qualid q) var.ts_id ->
        comb ~right:false f (term ~context t)
    | _ -> unsupported ()
  in
  match (bound t1.t_node, bound t2.t_node) with
  | Inf start, Sup stop | Sup stop, Inf start -> (start, stop)
  | _ -> unsupported ()

and case ~context (p, g, t) =
  let lhs, extra = pattern_ p in
  let formulas =
    match g with None -> extra | Some g -> term ~context g :: extra
  in
  let guard =
    match formulas with [] -> None | _ -> list_and formulas |> Option.some
  in
  Builder.case ~lhs ~guard ~rhs:(term ~context t)

and constant =
  let open Parse_uast in
  function
  | Pconst_integer (str, char_opt) -> Ppxlib.Pconst_integer (str, char_opt)
  | Pconst_char char -> Ppxlib.Pconst_char char
  | Pconst_string (str, loc) -> Ppxlib.Pconst_string (str, loc, None)
  | Pconst_float str -> Ppxlib.Pconst_float (str, None)

and binop ~loc ~context op t1 t2 =
  let term = term ~context in
  match op with
  | And ->
      let vt1 = gen_symbol ~prefix:"__t1" () in
      let vt2 = gen_symbol ~prefix:"__t2" () in
      [%expr
        let [%p pvar vt1] = [%e term t1] in
        let [%p pvar vt2] = [%e term t2] in
        [%e evar vt1] && [%e evar vt2]]
  | And_asym -> [%expr [%e term t1] && [%e term t2]]
  | Or ->
      let vt1 = gen_symbol ~prefix:"__t1" () in
      let vt2 = gen_symbol ~prefix:"__t2" () in
      [%expr
        let [%p pvar vt1] = [%e term t1] in
        let [%p pvar vt2] = [%e term t2] in
        [%e evar vt1] || [%e evar vt2]]
  | Or_asym -> [%expr [%e term t1] || [%e term t2]]
  | Implies -> [%expr (not [%e term t1]) || [%e term t2]]
  | Iff -> [%expr [%e term t1] = [%e term t2]]

and term ~context (t : Tast.term) : expression =
  let term = term ~context
  and loc = t.t_loc
  and is_implication = is_implication context
  and is_conjunction = is_conjunction context in
  let unsupported m = raise (W.Error (W.Unsupported m, loc)) in
  match t.t_node with
  (* ortac doesn't discriminate between bool and prop *)
  | Ttrue | TTrue -> [%expr true]
  | Tfalse | TFalse -> [%expr false]
  (* `Tyapply` constructor is for type-annotated variables *)
  | Tvar q | Ttyapply (q, _) ->
      let id = id_of_qualid q in
      let v =
        match Context.translate_stdlib id context with
        | None ->
            if Context.is_function id context then
              let f = Context.find_function id context in
              f
            else str_of_qualid q
        | Some f -> f
      in
      evar v
  | Tlet (x, t1, t2) ->
      [%expr
        let [%p pattern x] = [%e term t1] in
        [%e term t2]]
  | Tconst c -> econst @@ constant c
  | Tapply (f, a) ->
      let f = term f and a = term a in
      pexp_apply f [ (Nolabel, a) ]
  | Tquant
      ( quant,
        [ var ],
        (* Filtering the shape of supported quantification.
           Examples:
             `forall x. t0 /\ t1 -> pred`
             `exists x. t0 /\ t1 /\ pred` *)
        Tast.
          {
            t_node =
              Tapply
                ( {
                    t_node =
                      Tapply
                        ( op0 (* implication or conjunction *),
                          {
                            t_node =
                              Tapply ({ t_node = Tapply (op1, t0); _ }, t1);
                            _;
                          } );
                    _;
                  },
                  pred );
            _;
          } )
    when is_conjunction op1 && (is_conjunction op0 || is_implication op0) ->
      (match quant with
      | Tforall when is_implication op0 -> ()
      | Texists when is_conjunction op0 -> ()
      | _ -> unsupported "ill formed quantification");
      let start, stop = bounds ~context ~loc var t0 t1 in
      let pred = term pred in
      let quant =
        evar
          (if quant = Tforall then "Ortac_runtime.Z.forall"
           else "Ortac_runtime.Z.exists")
      in
      let x = str_of_ident var.ts_id in
      let func = pexp_fun Nolabel None (pvar x) pred in
      eapply quant [ start; stop; func ]
  | Tquant _ -> unsupported "quantification"
  | Tif (i, t, e) -> [%expr if [%e term i] then [%e term t] else [%e term e]]
  | Tset (ts, t) -> unsupported "set"
  | Ttuple terms -> pexp_tuple @@ List.map term terms
  | Tlambda (ps, t, _) ->
      efun (List.map (fun p -> (Nolabel, pattern p)) ps) (term t)
  | Trecord fields -> failwith "todo"
  | Tfield (t, q) ->
      pexp_field (term t) (lident (str_of_ident @@ id_of_qualid q))
  | Tattr (_attr, t) -> term t
  | Tcast (t, _) -> term t
  | Tscope (_q, t) -> term t
  | Told _ -> unsupported "old operator"

let term_with_catch ~context t =
  let exp = term ~context t in
  [%expr
    try [%e exp]
    with e ->
      raise (Ortac_runtime.Partial_function (e, [%e elocation t.t_loc]))]

let term_with_catch_bool ~context t =
  let open Tast in
  let open Types in
  if ty_equal t.t_ty ty_bool then term_with_catch ~context t
  else
    let exp = term ~context t in
    [%expr try [%e exp] with e -> false]

let core_type_of_ty_aux ~context f =
  let open Types in
  let lident_of_tysymbol ts =
    (match Context.translate_tystdlib ts context with
      | Some ty -> ty
      | None -> str_of_ident ts.ts_ident)
    |> Builder.lident
  in
  let rec arrow = function
    | [ a; b ] -> Builder.ptyp_arrow Nolabel a b
    | a :: bs -> Builder.ptyp_arrow Nolabel a (arrow bs)
    | _ -> invalid_arg "arrow"
  in
  let rec aux ty =
    match ty.ty_node with
    | Tyvar v -> f (str_of_ident v.tv_name)
    | Tyapp (ts, args) ->
        let args = List.map aux args in
        if is_ts_arrow ts then arrow args
        else if is_ts_tuple ts then Builder.ptyp_tuple args
        else Builder.ptyp_constr (lident_of_tysymbol ts) args
  in
  aux

let core_type_of_ty ~context = core_type_of_ty_aux ~context Builder.ptyp_var

let core_type_of_ty_with_subst ~context subst =
  core_type_of_ty_aux ~context (fun v ->
      Option.value ~default:(Builder.ptyp_var v) (subst v))

let core_type_of_tysymbol ~context ts =
  let lid =
    (match Context.translate_tystdlib ts context with
      | Some ty -> ty
      | None -> str_of_ident ts.Types.ts_ident)
    |> Builder.lident
  in
  let args =
    List.map
      (fun tv -> Builder.ptyp_var (str_of_ident tv.Types.tv_name))
      ts.ts_args
  in
  Builder.ptyp_constr lid args

let ocaml_type_decl_of_gospel_type_decl ~context td =
  let open Tast in
  let core_type_of_ty = core_type_of_ty ~context in
  let kind type_kind =
    match type_kind with
    | Pty_abstract -> Ptype_abstract
    | Pty_variant constr_decls ->
        Ptype_variant
          (List.map
             (fun cd ->
               if cd.cd_ld <> [] then
                 raise
                   W.(
                     Error
                       ( Unsupported "constructor with a record as argument",
                         cd.cd_loc ));
               let open Symbols in
               let name = noloc (str_of_ident (get_name cd.cd_cs))
               and args =
                 Pcstr_tuple (List.map core_type_of_ty (get_args cd.cd_cs))
               and res = None in
               constructor_declaration ~name ~args ~res)
             constr_decls)
    | Pty_record rec_decl ->
        Ptype_record
          (List.map
             (fun (ld : Symbols.lsymbol label_declaration) ->
               let name = noloc (str_of_ident (Symbols.get_name ld.ld_field))
               and mutable_ = Ppxlib.Immutable
               and type_ = core_type_of_ty (Symbols.get_value ld.ld_field) in
               label_declaration ~name ~mutable_ ~type_)
             rec_decl.rd_ldl)
  in
  let name = noloc (str_of_ident td.td_ts.ts_ident)
  and params =
    List.map
      (function
        | tvsymbol, (var, inj) ->
            (ptyp_var (str_of_ident tvsymbol.Types.tv_name), (var, inj)))
      td.td_params
  and cstrs =
    List.map
      (function
        | ty1, ty2, _ ->
            (core_type_of_ty ty1, core_type_of_ty ty2, Location.none))
      td.td_cstrs
  and kind = kind td.td_kind
  and private_ =
    match td.td_private with
    | Private -> Ppxlib.Private
    | Public -> Ppxlib.Public
  and manifest = Option.map core_type_of_ty td.td_manifest in
  type_declaration ~name ~params ~cstrs ~kind ~private_ ~manifest
