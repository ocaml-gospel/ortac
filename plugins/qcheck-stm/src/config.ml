open Gospel
open Ortac_core
open Ppxlib

module FrequenciesMap = struct
  module M = Map.Make (String)

  type t = (int * location) M.t

  let empty = M.empty
  let is_empty = M.is_empty

  let add label_loc v =
    let k = label_loc.txt and loc = label_loc.loc in
    M.add k (v, loc)

  let pop k m =
    let res = Option.value ~default:1 @@ Option.map fst @@ M.find_opt k m in
    (res, M.remove k m)

  let unused m = List.map (fun (k, (_freq, loc)) -> (k, loc)) @@ M.bindings m
end

module Frequencies = struct
  type t = {
    arb_cmd_seq : FrequenciesMap.t;
    arb_cmd_dom0 : FrequenciesMap.t;
    arb_cmd_dom1 : FrequenciesMap.t;
  }

  let empty =
    let open FrequenciesMap in
    { arb_cmd_seq = empty; arb_cmd_dom0 = empty; arb_cmd_dom1 = empty }

  type ('record, 'field) lens = {
    get : 'record -> 'field;
    set : 'field -> 'record -> 'record;
  }

  let seq =
    let get record = record.arb_cmd_seq
    and set field record = { record with arb_cmd_seq = field } in
    { get; set }

  let dom0 =
    let get record = record.arb_cmd_dom0
    and set field record = { record with arb_cmd_dom0 = field } in
    { get; set }

  let dom1 =
    let get record = record.arb_cmd_dom1
    and set field record = { record with arb_cmd_dom1 = field } in
    { get; set }

  let is_empty lens t = FrequenciesMap.is_empty @@ lens.get t

  let over lens f t =
    let old = lens.get t in
    lens.set (f old) t

  let add lens key value t = over lens (FrequenciesMap.add key value) t

  let pop lens key t =
    let map = lens.get t in
    let res, map = FrequenciesMap.pop key map in
    (res, lens.set map t)

  let unused lens t = FrequenciesMap.unused @@ lens.get t
end

type config_under_construction = {
  sut_core_type' : Ppxlib.core_type option;
  init_sut' : Ppxlib.expression option;
  arb_cmd' : Ppxlib.structure_item option;
  arb_cmd_seq' : Ppxlib.structure_item option;
  arb_cmd_dom0' : Ppxlib.structure_item option;
  arb_cmd_dom1' : Ppxlib.structure_item option;
  gen_mod' : Ppxlib.structure option;
  pp_mod' : Ppxlib.structure option;
  ty_mod' : Ppxlib.structure option;
  cleanup' : Ppxlib.structure_item option;
  frequencies' : Frequencies.t;
}

let config_under_construction =
  {
    sut_core_type' = None;
    init_sut' = None;
    arb_cmd' = None;
    arb_cmd_seq' = None;
    arb_cmd_dom0' = None;
    arb_cmd_dom1' = None;
    gen_mod' = None;
    pp_mod' = None;
    ty_mod' = None;
    frequencies' = Frequencies.empty;
    cleanup' = None;
  }

type t = {
  context : Context.t;
  sut_core_type : Ppxlib.core_type;
  init_sut : Ppxlib.expression;
  init_sut_txt : string;
  arb_cmd : Ppxlib.structure_item option;
  arb_cmd_seq : Ppxlib.structure_item option;
  arb_cmd_dom0 : Ppxlib.structure_item option;
  arb_cmd_dom1 : Ppxlib.structure_item option;
  gen_mod : Ppxlib.structure option; (* Containing custom QCheck generators *)
  pp_mod : Ppxlib.structure option; (* Containing custom pretty printers *)
  ty_mod : Ppxlib.structure option; (* Containing custom STM.ty extensions *)
  cleanup : Ppxlib.structure_item option;
  frequencies : Frequencies.t;
  module_prefix : string option;
  submodule : string option;
  domain : bool;
  count : int;
}

let mk_config context module_prefix submodule domain count cfg_uc =
  let open Reserr in
  let* sut_core_type =
    of_option
      ~default:(Incomplete_configuration_module `Sut, Location.none)
      cfg_uc.sut_core_type'
  and* init_sut =
    of_option
      ~default:(Incomplete_configuration_module `Init_sut, Location.none)
      cfg_uc.init_sut'
  in
  let init_sut_txt = Fmt.str "%a" Pprintast.expression init_sut
  and arb_cmd = cfg_uc.arb_cmd'
  and arb_cmd_seq = cfg_uc.arb_cmd_seq'
  and arb_cmd_dom0 = cfg_uc.arb_cmd_dom0'
  and arb_cmd_dom1 = cfg_uc.arb_cmd_dom1'
  and gen_mod = cfg_uc.gen_mod'
  and pp_mod = cfg_uc.pp_mod'
  and ty_mod = cfg_uc.ty_mod'
  and frequencies = cfg_uc.frequencies'
  and cleanup = cfg_uc.cleanup' in
  ok
    {
      context;
      sut_core_type;
      init_sut;
      init_sut_txt;
      arb_cmd;
      arb_cmd_seq;
      arb_cmd_dom0;
      arb_cmd_dom1;
      gen_mod;
      pp_mod;
      ty_mod;
      frequencies;
      cleanup;
      module_prefix;
      submodule;
      domain;
      count;
    }

let get_sut_type_name config =
  let open Ppxlib in
  match config.sut_core_type.ptyp_desc with
  | Ppxlib.Ptyp_constr (lid, _) -> lid.txt
  | _ -> failwith "unreachable case in get_sut_type_name"

let get_sut_type_name_str config =
  Ppxlib.Longident.last_exn (get_sut_type_name config)

let is_sut config ty =
  let sut_type_name = get_sut_type_name config in
  let open Ppxlib in
  match ty.ptyp_desc with
  | Ptyp_constr (lid, _) -> lid.txt = sut_type_name
  | _ -> false

let does_return_sut config ty =
  let rec aux ty =
    match ty.ptyp_desc with
    | Ptyp_arrow (_, _, r) -> aux r
    | Ptyp_constr _ -> is_sut config ty
    | _ -> false
  in
  aux ty

let dump ppf t =
  Fmt.(
    pf ppf "sut_core_type: %a; init_sut: %a@." Ppxlib_ast.Pprintast.expression
      t.init_sut Ppxlib_ast.Pprintast.core_type t.sut_core_type)

let rec acceptable_type_parameter param =
  let open Ppxlib in
  let open Reserr in
  let str = Fmt.str "%a" Ppxlib_ast.Pprintast.core_type param in
  match param.ptyp_desc with
  | Ptyp_constr (_, cts) ->
      let* _ = promote_map acceptable_type_parameter cts in
      ok ()
  | Ptyp_tuple args ->
      let* _ = promote_map acceptable_type_parameter args in
      ok ()
  | Ptyp_var _ | Ptyp_any ->
      error (Type_parameter_not_instantiated str, Location.none)
  | _ -> error (Type_not_supported_for_sut_parameter str, Location.none)

let core_type_is_a_well_formed_sut (core_type : Ppxlib.core_type) =
  let open Ppxlib in
  let open Reserr in
  match core_type.ptyp_desc with
  | Ptyp_constr (_lid, cts) ->
      let* _ = promote_map acceptable_type_parameter cts in
      ok ()
  | _ ->
      let str = Fmt.str "%a" Ppxlib_ast.Pprintast.core_type core_type in
      error (Sut_type_not_supported str, Location.none)

(* Inspect value definition in config module in order to collect information
   about:
   - the definition of the [init_sut] function *)
let value_bindings cfg_uc =
  let open Reserr in
  let aux cfg_uc vb =
    let open Ppxlib in
    match vb.pvb_pat.ppat_desc with
    | Ppat_var s when String.equal "init_sut" s.txt ->
        let init_sut' = Some vb.pvb_expr in
        ok { cfg_uc with init_sut' }
    | Ppat_var s when String.equal "cleanup" s.txt ->
        let cleanup' =
          Option.some @@ Ortac_core.Builder.pstr_value Nonrecursive [ vb ]
        in
        ok { cfg_uc with cleanup' }
    | Ppat_var s when String.equal "arb_cmd" s.txt ->
        let arb_cmd' =
          Option.some @@ Ortac_core.Builder.pstr_value Nonrecursive [ vb ]
        in
        ok { cfg_uc with arb_cmd' }
    | Ppat_var s when String.equal "arb_cmd" s.txt ->
        let arb_cmd_seq' =
          Option.some @@ Ortac_core.Builder.pstr_value Nonrecursive [ vb ]
        in
        ok { cfg_uc with arb_cmd_seq' }
    | Ppat_var s when String.equal "arb_cmd_dom0" s.txt ->
        let arb_cmd_dom0' =
          Option.some @@ Ortac_core.Builder.pstr_value Nonrecursive [ vb ]
        in
        ok { cfg_uc with arb_cmd_dom0' }
    | Ppat_var s when String.equal "arb_cmd_dom1" s.txt ->
        let arb_cmd_dom1' =
          Option.some @@ Ortac_core.Builder.pstr_value Nonrecursive [ vb ]
        in
        ok { cfg_uc with arb_cmd_dom1' }
    | _ -> ok cfg_uc
  in
  fold_left aux cfg_uc

(* Inspect type definition in config module in order to collect information
   about:
   - the definition of the [sut] type *)
let type_declarations cfg_uc =
  let open Reserr in
  let aux cfg_uc (td : type_declaration) =
    let open Ppxlib in
    if String.equal "sut" td.ptype_name.txt then
      let* manifest =
        of_option
          ~default:
            ( Sut_type_not_supported (Fmt.str "%a" Pprintast.type_declaration td),
              td.ptype_loc )
          td.ptype_manifest
      in
      let* () = core_type_is_a_well_formed_sut manifest in
      ok { cfg_uc with sut_core_type' = Some manifest }
    else ok cfg_uc
  in
  fold_left aux cfg_uc

(* Inspect module definition in config module in order to collect information
   about:
     - the custom [QCheck] generators
     - the custom [STM] pretty printers
     - the custom [STM.ty] extensions and function constructors *)
let module_binding cfg_uc (mb : Ppxlib.module_binding) =
  let open Reserr in
  let get_structure name mb =
    match mb.pmb_expr.pmod_desc with
    | Pmod_structure structure
    (* there is no need to go further, module constraints of module
       constraints doesn't make sense *)
    | Pmod_constraint ({ pmod_desc = Pmod_structure structure; _ }, _) ->
        ok structure
    | _ -> error (Not_a_structure name, Location.none)
  in
  match mb.pmb_name.txt with
  | Some name when String.equal "Gen" name ->
      let* content = get_structure name mb in
      ok { cfg_uc with gen_mod' = Some content }
  | Some name when String.equal "Pp" name ->
      let* content = get_structure name mb in
      ok { cfg_uc with pp_mod' = Some content }
  | Some name when String.equal "Ty" name ->
      let* content = get_structure name mb in
      ok { cfg_uc with ty_mod' = Some content }
  | Some name when String.equal "Frequencies" name ->
      let* content = get_structure name mb in
      let open Ast_pattern in
      let destruct =
        pstr_value drop
          (value_binding ~constraint_:drop ~pat:(ppat_var __') ~expr:(eint __)
          ^:: nil)
      and error stri =
        let loc = stri.pstr_loc in
        error (Ill_formed_frequency, loc)
      in
      let aux acc stri =
        parse destruct Location.none
          ~on_error:(Fun.const @@ error stri)
          stri
          (fun name freq ->
            ok
              {
                acc with
                frequencies' = Frequencies.(add seq name freq acc.frequencies');
              })
      in
      fold_left aux cfg_uc content
  | _ -> ok cfg_uc

let scan_config cfg_uc config_mod =
  let open Reserr in
  let* ic =
    try ok @@ open_in config_mod
    with _ -> error (No_configuration_file config_mod, Location.none)
  in
  let lb = Lexing.from_channel ic in
  let () = Lexing.set_filename lb config_mod in
  let* ast =
    try ok @@ Ppxlib.Parse.implementation lb
    with _ ->
      error
        ( Syntax_error_in_config_module config_mod,
          Location.
            {
              loc_start = lb.lex_start_p;
              loc_end = lb.lex_curr_p;
              loc_ghost = false;
            } )
  in
  close_in ic;
  let aux cfg_uc (str : structure_item) =
    match str.pstr_desc with
    | Pstr_eval (_, _) -> ok cfg_uc
    | Pstr_value (_, xs) -> value_bindings cfg_uc xs
    | Pstr_primitive _ -> ok cfg_uc
    | Pstr_type (_, xs) -> type_declarations cfg_uc xs
    | Pstr_typext _ -> ok cfg_uc
    | Pstr_exception _ -> ok cfg_uc
    | Pstr_module mb -> module_binding cfg_uc mb
    | Pstr_recmodule _ -> ok cfg_uc
    | Pstr_modtype _ -> ok cfg_uc
    | Pstr_open _ -> ok cfg_uc
    | Pstr_class _ -> ok cfg_uc
    | Pstr_class_type _ -> ok cfg_uc
    | Pstr_include _ -> ok cfg_uc
    | Pstr_attribute _ -> ok cfg_uc
    | Pstr_extension (_, _) -> ok cfg_uc
  in
  fold_left aux cfg_uc ast

let init gospel config_module module_prefix submodule domain count =
  let open Reserr in
  try
    let module_name = Utils.module_name_of_path gospel in
    Parser_frontend.parse_ocaml_gospel gospel |> Utils.type_check [] gospel
    |> fun (env, sigs) ->
    assert (List.length env = 1);
    let namespace = List.hd env in
    let context = Context.init module_name namespace in
    let add ctx s =
      (* we add to the context the pure OCaml values and the functions and
         predicates with a body *)
      match s.Tast.sig_desc with
      | Sig_val ({ vd_name; vd_spec = Some { sp_pure = true; _ }; _ }, _) ->
          let ls = Context.get_ls ctx [ vd_name.id_str ] in
          Context.add_function ls vd_name.id_str ctx
      | Sig_function { fun_ls; fun_def = Some _; _ } ->
          Context.add_function fun_ls fun_ls.ls_name.id_str ctx
      | _ -> ctx
    in
    let context = List.fold_left add context sigs in
    let* config =
      scan_config config_under_construction config_module
      >>= mk_config context module_prefix submodule domain count
    in
    ok (sigs, config)
  with Gospel.Warnings.Error (l, k) ->
    error (Ortac_core.Warnings.GospelError k, l)
