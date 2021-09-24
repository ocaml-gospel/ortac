module W = Warnings
open Ppxlib
open Gospel

module Make (B : Backend.S) = struct
  open Builder
  module T = Translation

  let term_printer (spec : Tast.val_spec) (t : Tterm.term) =
    match t.t_loc with
    | None -> Fmt.str "%a" Tterm.print_term t
    | Some loc -> (
        try
          String.sub spec.sp_text
            (loc.loc_start.pos_cnum - spec.sp_loc.loc_start.pos_cnum)
            (loc.loc_end.pos_cnum - loc.loc_start.pos_cnum)
        with Invalid_argument _ -> Fmt.str "%a" Tterm.print_term t)

  let of_gospel_args args =
    let to_string x = Fmt.str "%a" Tast.Ident.pp x.Tterm.vs_name in
    let to_ty x = x.Tterm.vs_ty in
    List.fold_right
      (fun arg (eargs, pargs, targs) ->
        match arg with
        | Tast.Lunit ->
            ( (Nolabel, eunit) :: eargs,
              (Nolabel, punit) :: pargs,
              Ttypes.ty_unit :: targs )
        | Tast.Lnone x ->
            let s = to_string x in
            let t = to_ty x in
            ((Nolabel, evar s) :: eargs, (Nolabel, pvar s) :: pargs, t :: targs)
        | Tast.Loptional x ->
            let s = to_string x in
            let t = to_ty x in
            ( (Optional s, evar s) :: eargs,
              (Nolabel, pvar s) :: pargs,
              t :: targs )
        | Tast.Lnamed x ->
            let s = to_string x in
            let t = to_ty x in
            ( (Labelled s, evar s) :: eargs,
              (Labelled s, pvar s) :: pargs,
              t :: targs )
        | Tast.Lghost _ -> (eargs, pargs, targs))
      args ([], [], [])

  let ts_of_ty ty =
    match ty.Ttypes.ty_node with
    | Ttypes.Tyvar _ -> None
    | Tyapp (ts, _) -> Some ts

  let value ~driver ~invariants_table (vd : Tast.val_description) =
    let process (spec : Tast.val_spec) =
      let term_printer = term_printer spec in
      (* Declaration location *)
      let loc = vd.vd_loc in
      let setup_expr, register_name = T.mk_setup loc vd.vd_name.id_str in
      let register_name = evar register_name in
      (* Arguments *)
      let eargs, pargs, targs = of_gospel_args spec.sp_args in
      (* Returned pattern *)
      let ret_pat, ret_expr = T.returned_pattern spec.sp_ret in
      let all_terms =
        spec.sp_post @ spec.sp_pre
        @ List.fold_left
            (fun acc (_, ptl) -> List.map snd ptl @ acc)
            [] spec.sp_xpost
      in
      let olds, old_defs = Old_translation.process_olds ~driver all_terms in
      let input_invariants_checks state next =
        List.map2
          (fun (_, eh) th ->
            match ts_of_ty th with
            | None -> eunit
            | Some ts ->
                Hashtbl.find_all invariants_table ts
                |> List.map (fun check_function ->
                       eapply check_function [ register_name; evar state; eh ])
                |> esequence)
          eargs targs
        |> esequence
        |> fun e -> pexp_sequence e next
      in
      let pre_checks =
        T.mk_pre_checks ~driver ~olds ~register_name ~term_printer spec.sp_pre
      in
      let let_call =
        T.mk_call ~driver ~olds ~register_name ~term_printer ret_pat loc
          vd.vd_name.id_str spec.sp_xpost eargs
      in
      let post_checks =
        T.mk_post_checks ~driver ~olds ~register_name ~term_printer spec.sp_post
      in
      let body =
        efun pargs @@ setup_expr @@ old_defs
        @@ input_invariants_checks "Pre"
        @@ pre_checks @@ let_call
        @@ input_invariants_checks "Post"
        @@ post_checks @@ ret_expr
      in
      [%stri let [%p pvar vd.vd_name.id_str] = [%e body]]
    in
    match vd.vd_spec with None -> [] | Some spec -> [ process spec ]

  let constant ~driver (vd : Tast.val_description) =
    let process spec =
      let term_printer = term_printer spec in
      let loc = vd.vd_loc in
      let setup_expr, register_name = T.mk_setup loc vd.vd_name.id_str in
      let register_name = evar register_name in
      let post_checks =
        T.mk_post_checks ~driver ~olds:(Hashtbl.create 0) ~register_name
          ~term_printer spec.sp_post
      in
      let body = setup_expr @@ post_checks @@ evar vd.vd_name.id_str in
      [%stri let [%p pvar vd.vd_name.id_str] = [%e body]]
    in
    match vd.vd_spec with None -> [] | Some spec -> [ process spec ]

  let types ~driver ~invariants_table =
    let process (td : Tast.type_declaration) (ts : Tast.type_spec) =
      (* Issue a warning for models and extract them for the term translation *)
      let models =
        List.map
          (fun (m, _) ->
            W.(register (Unsupported_model m.Tterm.ls_name.id_str, td.td_loc));
            m)
          ts.ty_fields
      in
      (* Create a name for the checking function *)
      let func_name =
        Fmt.kstr
          (fun prefix -> gen_symbol ~prefix ())
          "__invariants_%s" td.td_ts.ts_ident.id_str
      in
      (* Create a name for the type instance *)
      let vs_name = Tterm.Ident.create (gen_symbol ~prefix:"__instance" ()) in
      let instance_name = Fmt.str "%a" Identifier.Ident.pp vs_name in
      (* Variables passed by the caller for error reporting *)
      let register_name = gen_symbol ~prefix:"__register" () in
      let state_name = gen_symbol ~prefix:"__state" () in
      let term_printer = Fmt.str "%a" Tterm.print_term in
      (* The body contains the evaluation of the term *)
      let body =
        T.mk_invariants_checks ~models ~driver ~state:(evar state_name)
          ~typ:td.td_ts.ts_ident.id_str ~instance:vs_name
          ~register_name:(evar register_name) ~term_printer ts.ty_invariants
      in
      (* We add the function name so it can be called by functions whenever
         needed *)
      Hashtbl.add invariants_table td.td_ts (evar func_name);
      [%stri
        let [%p pvar func_name] =
         fun [%p pvar register_name] [%p pvar state_name]
             [%p pvar instance_name] ->
          [%e body]]
    in
    List.filter_map (fun (td : Tast.type_declaration) ->
        Drv.add_type_definition driver td.td_ts td.td_kind;
        match td.td_spec with None -> None | Some ts -> Some (process td ts))

  let function_ ~driver (func : Tast.function_) =
    let loc = func.fun_loc in
    match func.fun_def with
    | None ->
        W.(register (Unsupported "uninterpreted function or predicate", loc));
        []
    | Some def -> (
        let name = gen_symbol ~prefix:("__" ^ func.fun_ls.ls_name.id_str) () in
        let pargs =
          List.map
            (fun vs ->
              (Nolabel, pvar (Fmt.str "%a" Identifier.Ident.pp vs.Tterm.vs_name)))
            func.fun_params
        in
        (* This is needed for recursive functions; ideally the driver should be
           functional.*)
        Drv.add_translation driver func.fun_ls name;
        let recursive = if func.fun_rec then Recursive else Nonrecursive in
        match T.mk_function_def ~driver def with
        | None ->
            Drv.remove_translation driver func.fun_ls;
            []
        | Some expr ->
            let body = efun pargs expr in
            [
              pstr_value recursive [ value_binding ~pat:(pvar name) ~expr:body ];
            ])

  let signature module_name env s =
    let driver = Drv.v env in
    let invariants_table = Hashtbl.create 0 in
    let declarations =
      List.map
        (fun (sig_item : Tast.signature_item) ->
          match sig_item.sig_desc with
          | Sig_val (decl, true) ->
              W.register (W.Unsupported "ghost value", decl.vd_loc);
              []
          | Sig_val (decl, false) when decl.vd_args <> [] ->
              value ~invariants_table ~driver decl
          | Sig_val (decl, _ghost) -> constant ~driver decl
          | Sig_type (_, _, true) ->
              W.register (W.Unsupported "ghost type", sig_item.sig_loc);
              []
          | Sig_type (_rec_flag, tl, false) ->
              types ~driver ~invariants_table tl
          | Sig_function func -> function_ ~driver func
          | Sig_axiom axiom ->
              W.register (W.Unsupported "axiom", axiom.Tast.ax_loc);
              []
          | _ -> [])
        s
      |> List.flatten
    in
    W.report ();
    let include_lib =
      pmod_ident (lident module_name) |> include_infos |> pstr_include
    in
    B.prelude @ (include_lib :: declarations)
end
