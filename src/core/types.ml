open Gospel

module Mutability = struct
  let max m n =
    let open Translated in
    match (m, n) with
    | Mutable, _ | _, Mutable -> Mutable
    | Dependant f, _ | _, Dependant f -> Dependant f
    | Unknown, _ | _, Unknown -> Unknown
    | Immutable, Immutable -> Immutable

  let min_mut = Translated.Immutable

  let tysymbol ~driver (ts : Ttypes.tysymbol) =
    (* To determine the mutability of a `tysymbol`, we look in the driver. *)
    match Drv.get_type ts driver with
    | None ->
        (* If the driver doesn't know about this `tysymbol`, the mutability is `Unknown`.
           Note that of the type is parametric we wait for the instantiation to deliver the
           information *)
        if List.length ts.ts_args > 0 then
          Translated.Dependant (fun _ -> Unknown)
        else Translated.Unknown
    | Some t -> t.mutable_

  let alpha (ty : Ttypes.ty) =
    match ty.ty_node with Tyvar _ -> true | _ -> false

  let rec ty ~driver (t : Ttypes.ty) =
    match t.ty_node with
    | Tyvar _ ->
        (* A `Tyvar` is an alpha *)
        Translated.Unknown
    | Tyapp (ts, tyl) when Ttypes.is_ts_tuple ts ->
        (* XXX not sure about that decision... *)
        (* In the presence of a tuple there are three cases
           1. There is at least an alpha in its element but another element is already known to be mutable
           2. There is at least an alpha but none element is known to be mutable
           3. All the elements are known types *)
        if List.exists alpha tyl then
          max
            (List.map (ty ~driver) tyl |> List.fold_left max min_mut)
            (Translated.Dependant (fun tyl -> List.fold_right max tyl min_mut))
        else List.map (ty ~driver) tyl |> List.fold_left max min_mut
    | Tyapp (ts, tyl) when List.length tyl = 0 ->
        (* If `tyl` is empty, we just look at `ts`. The mutability can't be `Dependant` *)
        tysymbol ~driver ts
    | Tyapp (ts, tyl) -> (
        (* It the list is not empty, that means that `ts` is a parametric type and
           its mutability is `Dependant` *)
        match tysymbol ~driver ts with
        | Translated.Dependant f as dep ->
            (* It there is still an alpha in the parameters, the mutability is still `Dependant`,
               otherwise, we apply the function embebed in the `Dependant` to the parameters *)
            if List.exists alpha tyl then dep else f (List.map (ty ~driver) tyl)
        | _ -> assert false)

  let lsymbol ~driver (ls : Tterm.lsymbol) =
    (* To determine the mutability of a `lsymbol` we look at its `ls_value`
       which is a `Ttypes.ty option`.
       If there is none, the mutability is unknown *)
    Option.fold ~none:Translated.Unknown ~some:(ty ~driver) ls.ls_value

  let constructor_declaration ~driver (cd : Tast.constructor_decl) =
    (* The mutability of a constructor is the max od the mutability of its argument *)
    List.map (ty ~driver) cd.cd_cs.ls_args |> List.fold_left max min_mut

  let field_declaration ~driver (ld : Tterm.lsymbol Tast.label_declaration) =
    (* A record field is mutable if it is annotated as mutable, if not we look
       at the lsymbol it contains i.e. the field itself. *)
    match ld.ld_mut with
    | Tast.Mutable -> Translated.Mutable
    | Tast.Immutable -> lsymbol ~driver ld.ld_field

  let type_declaration ~driver (td : Tast.type_declaration) =
    (* To determine the mutability of a type declaration, we check
       whether it is an alias. *)
    match td.td_ts.ts_alias with
    | Some alias ->
        (* An alias it a Ttypes.ty, e.g. `type t = int` *)
        ty ~driver alias
    | None -> (
        match td.td_kind with
        (* We don't have any information on an abstract type *)
        | Pty_abstract -> Translated.Unknown
        (* The mutability of a variant is the max of the mutability of its contructor *)
        | Pty_variant cdl ->
            List.map (constructor_declaration ~driver) cdl
            |> List.fold_left max min_mut
        | Pty_record rd ->
            (* The mutability of a record is the max of the mutability of its fields *)
            List.map (field_declaration ~driver) rd.rd_ldl
            |> List.fold_left max min_mut
        | Pty_open -> Translated.Unknown (* ? *))

  let mutable_model ~driver (ty_fields : (Tterm.lsymbol * bool) list) =
    List.map
      (* if a model is annotated as mutable, it is mutable, if not we look at
         the type of the model *)
        (fun (ls, b) -> if b then Translated.Mutable else lsymbol ~driver ls)
      ty_fields
    (* the mutability of a type is here the max of the mutability of its models *)
    |> List.fold_left max min_mut

  let type_spec ~driver (spec : Tast.type_spec) =
    (* To determine the mutability of a type according to its specification
       we check whether it is annotated with an ephemeral, and if not we
       look at its models *)
    if spec.ty_ephemeral then Translated.Mutable
    else mutable_model ~driver spec.ty_fields
end

module W = Warnings
open Ppxlib

let result_of_list f l =
  let rec aux = function
    | [] -> Ok []
    | x :: t -> (
        match f x with
        | Error _ as e -> e
        | Ok x ->
            let t = aux t in
            Result.map (fun t -> x :: t) t)
  in
  aux l

module Comparison = struct
  let core_type =
    [
      "unit";
      "bool";
      "char";
      "string";
      "float";
      "integer";
      "option";
      "list";
      "array";
      "ref";
      "seq";
      "bag";
      "set";
    ]

  module Common = struct
    let var_i ~prefix l () =
      List.mapi (fun i _ -> gen_symbol ~prefix:(prefix ^ Int.to_string i) ()) l

    let vars ~prefix l () =
      List.map (fun s -> gen_symbol ~prefix:(prefix ^ s) ()) l

    let item ~loc cmp elt_a elt_b next =
      let open Ast_builder.Make (struct
        let loc = loc
      end) in
      let res = gen_symbol ~prefix:"__res" () in
      [%expr
        let [%e pvar res] = [%e cmp] [%e evar elt_a] [%e evar elt_b] in
        if [%e evar res] <> 0 then [%e evar res] else [%e next]]

    let rec fold ~loc cmps elts_a elts_b =
      match (cmps, elts_a, elts_b) with
      | [], [], [] -> [%expr 0]
      | c :: cs, elt_a :: elts_a, elt_b :: elts_b ->
          item ~loc c elt_a elt_b (fold ~loc cs elts_a elts_b)
      | _, _, _ -> assert false
  end

  module Variant = struct
    (** In this module we define some auxiliary functions for generating
        comparison about variants *)

    let constructor_pattern ~loc (name, constructor) =
      (* build the pattern corresponding to the constructor and return it along
         with the list of variable names occuring in it *)
      let open Ast_builder.Make (struct
        let loc = loc
      end) in
      let cstr = Builder.lident name in
      let names, rec_pat =
        match constructor with
        | Translated.Named fields ->
            let fields = List.map fst fields in
            let names = Common.vars ~prefix:"__" fields () in
            let fields_pat =
              List.map2 (fun f n -> (Builder.lident f, pvar n)) fields names
            in
            (names, Some (ppat_record fields_pat Closed))
        | Translated.Unnamed args ->
            let names = Common.var_i ~prefix:"__elt" args () in
            (names, ppat_tuple_opt (List.map pvar names))
      in
      (names, ppat_construct cstr rec_pat)

    let lhs ~loc (c0, arg0) (c1, args1) =
      let open Ast_builder.Make (struct
        let loc = loc
      end) in
      (* we need to name the arguments od the constructors only if they are the same *)
      if c0 = c1 then
        let var0, pat0 = constructor_pattern ~loc (c0, arg0) in
        let var1, pat1 = constructor_pattern ~loc (c1, args1) in
        (ppat_tuple [ pat0; pat1 ], var0, var1)
      else
        let pat0 = ppat_construct (Builder.lident c0) None in
        let pat1 = ppat_construct (Builder.lident c1) None in
        (ppat_tuple [ pat0; pat1 ], [], [])

    let rhs ~loc cmps var0 var1 =
      (* As we will check the first argument against the second, we know that if
         we don't have any names for the arguments the constructor [var0] if less
         than the constructor [var1] *)
      if List.length var0 = 0 then [%expr -1]
      else Common.fold ~loc cmps var0 var1

    let case ~loc cmps c0 c1 =
      let open Ast_builder.Make (struct
        let loc = loc
      end) in
      let lhs, var0, var1 = lhs ~loc c0 c1 in
      let rhs = rhs ~loc cmps var0 var1 in
      case ~lhs ~guard:None ~rhs

    let cmp_cases ~loc = List.map2 (fun cmp c -> case ~loc cmp c c)

    let inf_cases ~loc constructors =
      (* check a.(i) against b.(j) for all i < j *)
      let a = Array.of_list constructors in
      let b = Array.of_list constructors in
      let stop = Array.length a in
      let rec loop i j =
        if i = stop then []
        else if j = stop then loop (succ i) (i + 2)
        else case ~loc [] a.(i) b.(j) :: loop i (succ j)
      in
      loop 0 1

    let catch_all ~loc =
      let open Ast_builder.Make (struct
        let loc = loc
      end) in
      [
        case ~lhs:(ppat_tuple [ ppat_any; ppat_any ]) ~guard:None ~rhs:[%expr 1];
      ]
  end

  let rec core ~loc name args =
    let open Ast_builder.Make (struct
      let loc = loc
    end) in
    match name with
    | "unit" | "bool" | "int" | "char" | "string" | "float" | "integer" ->
        Ok [%expr compare]
    | "option" ->
        let build cmp = [%expr Option.compare [%e cmp]] in
        Result.map build (derive (List.hd args))
    | "list" ->
        let build cmp = [%expr List.compare [%e cmp]] in
        Result.map build (derive (List.hd args))
    | "array" ->
        let build cmp = [%expr Ortac_runtime.Array.compare [%e cmp]] in
        Result.map build (derive (List.hd args))
    | "ref" ->
        let a = gen_symbol ~prefix:"__a" () in
        let b = gen_symbol ~prefix:"__b" () in
        let build cmp =
          [%expr
            fun [%p pvar a] [%p pvar b] -> [%e cmp] ![%e evar a] ![%e evar b]]
        in
        Result.map build (derive (List.hd args))
    | "seq" -> Error (W.Unsupported_comparison "seq")
    | "bag" -> Error (W.Unsupported_comparison "bag")
    | "set" -> Error (W.Unsupported_comparison "set")
    | _ -> assert false

  and synonyms ~loc (constructor : Translated.type_) args =
    if List.mem constructor.name core_type then core ~loc constructor.name args
    else
      match args with
      | [] -> derive constructor
      | _ -> Error (W.Unsupported_comparison constructor.name)
  (* XXX we don't know yet how to do it *)

  and constructor = function
    | Translated.Named fields -> result_of_list (fun (_, t) -> derive t) fields
    | Translated.Unnamed args -> result_of_list derive args

  and variant ~loc constructors =
    let open Ast_builder.Make (struct
      let loc = loc
    end) in
    match result_of_list (fun (_, c) -> constructor c) constructors with
    | Error e -> Error e
    | Ok cmps ->
        let a, b = (gen_symbol ~prefix:"__a" (), gen_symbol ~prefix:"__b" ()) in
        let cases =
          Variant.(
            cmp_cases ~loc cmps constructors
            @ inf_cases ~loc constructors
            @ catch_all ~loc)
        in
        let case_split = pexp_match (pexp_tuple [ evar a; evar b ]) cases in
        Ok [%expr fun [%p pvar a] [%p pvar b] -> [%e case_split]]

  and tuple ~loc types =
    let open Ast_builder.Make (struct
      let loc = loc
    end) in
    match result_of_list derive types with
    | Error e -> Error e
    | Ok cmps ->
        let vars_a = Common.var_i ~prefix:"__elt_a" cmps () in
        let vars_b = Common.var_i ~prefix:"__elt_b" cmps () in
        let body = Common.fold ~loc cmps vars_a vars_b in
        let pat_a = ppat_tuple (List.map pvar vars_a) in
        let pat_b = ppat_tuple (List.map pvar vars_b) in
        Ok [%expr fun [%p pat_a] [%p pat_b] -> [%e body]]

  and record ~loc fields =
    let open Ast_builder.Make (struct
      let loc = loc
    end) in
    match result_of_list (fun (_, t) -> derive t) fields with
    | Error e -> Error e
    | Ok cmps ->
        let fields = List.map fst fields in
        let fields_lidents = List.map Builder.lident fields in
        let names_a = Common.vars ~prefix:"__field_a" fields () in
        let names_b = Common.vars ~prefix:"__field_b" fields () in
        let pat_a =
          ppat_record
            (List.combine fields_lidents (List.map pvar names_a))
            Closed
        in
        let pat_b =
          ppat_record
            (List.combine fields_lidents (List.map pvar names_b))
            Closed
        in
        let body = Common.fold ~loc cmps names_a names_b in
        Ok [%expr fun [%p pat_a] [%p pat_b] -> [%e body]]

  and derive (t : Translated.type_) =
    let loc = t.loc in
    match t.kind with
    | Translated.Abstract -> Error (W.Unsupported_comparison "abstract")
    | Translated.Alpha -> Error (W.Unsupported_comparison "polymorphic")
    | Translated.Core args -> core ~loc t.name args
    | Translated.Synonyms (c, a) -> synonyms ~loc c a
    | Translated.Variant cstrs -> variant ~loc cstrs
    | Translated.Record fields -> record ~loc fields
    | Translated.Tuple elts -> tuple ~loc elts
end
