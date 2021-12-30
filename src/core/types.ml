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

module Common = struct
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

  let var_i ~prefix l () =
    List.mapi (fun i _ -> gen_symbol ~prefix:(prefix ^ Int.to_string i) ()) l

  let vars ~prefix l () =
    List.map (fun s -> gen_symbol ~prefix:(prefix ^ s) ()) l

  let rec fold ~loc ~combinator ~operators elts_a elts_b =
    let open Ast_builder.Make (struct
      let loc = loc
    end) in
    match (operators, elts_a, elts_b) with
    | [ c ], [ elt_a ], [ elt_b ] ->
        pexp_apply c [ (Nolabel, evar elt_a); (Nolabel, evar elt_b) ]
    | c :: cs, elt_a :: elts_a, elt_b :: elts_b ->
        combinator ~loc c elt_a elt_b
          (fold ~loc ~combinator ~operators:cs elts_a elts_b)
    | _, _, _ -> assert false

  let synonyms ~loc ~derive ~core (constructor : Translated.type_) args =
    if List.mem constructor.name core_type then core ~loc constructor.name args
    else
      match args with
      | [] -> derive constructor
      | _ -> Error (W.Unsupported_equality constructor.name)
  (* XXX we don't know yet how to do it *)

  let record_pattern ~loc ~fields ~vars =
    let open Ast_builder.Make (struct
      let loc = loc
    end) in
    let fields = List.map Builder.lident fields in
    let vars = List.map pvar vars in
    ppat_record (List.combine fields vars) Closed

  let record_aux ~loc ~combinator ~fields operators =
    let open Ast_builder.Make (struct
      let loc = loc
    end) in
    let fields = List.map fst fields in
    let names_a = vars ~prefix:"__field_a" fields () in
    let names_b = vars ~prefix:"__field_b" fields () in
    let pat_a = record_pattern ~loc ~fields ~vars:names_a in
    let pat_b = record_pattern ~loc ~fields ~vars:names_b in
    let body = fold ~loc ~combinator ~operators names_a names_b in
    [%expr fun [%p pat_a] [%p pat_b] -> [%e body]]

  let record ~loc ~derive ~combinator fields =
    result_of_list (fun (_, t) -> derive t) fields
    |> Result.map (record_aux ~loc ~combinator ~fields)

  let tuple_aux ~loc ~combinator operators =
    let open Ast_builder.Make (struct
      let loc = loc
    end) in
    let vars_a = var_i ~prefix:"__elt_a" operators () in
    let vars_b = var_i ~prefix:"__elt_b" operators () in
    let body = fold ~loc ~combinator ~operators vars_a vars_b in
    let pat_a = ppat_tuple (List.map pvar vars_a) in
    let pat_b = ppat_tuple (List.map pvar vars_b) in
    [%expr fun [%p pat_a] [%p pat_b] -> [%e body]]

  let tuple ~loc ~derive ~combinator types =
    result_of_list derive types |> Result.map (tuple_aux ~loc ~combinator)

  let constructor fct = function
    | Translated.Named fields -> result_of_list (fun (_, t) -> fct t) fields
    | Translated.Unnamed args -> result_of_list fct args

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
          let names = vars ~prefix:"__" fields () in
          let fields_pat =
            List.map2 (fun f n -> (Builder.lident f, pvar n)) fields names
          in
          (names, Some (ppat_record fields_pat Closed))
      | Translated.Unnamed args ->
          let names = var_i ~prefix:"__elt" args () in
          (names, ppat_tuple_opt (List.map pvar names))
    in
    (names, ppat_construct cstr rec_pat)

  let constructor_any_pattern ~loc (name, constructor) =
    (* build the pattern corresponding to the constructor when we don't
       need to have variable for its arguments *)
    let open Ast_builder.Make (struct
      let loc = loc
    end) in
    let cstr = Builder.lident name in
    let rec_pat =
      match constructor with
      | Translated.Named _fields -> Some ppat_any
      | Translated.Unnamed args ->
          if List.length args = 0 then None
          else Some (ppat_tuple (List.map (fun _ -> ppat_any) args))
    in
    ppat_construct cstr rec_pat

  let lhs ~loc (c0, arg0) (c1, args1) =
    let open Ast_builder.Make (struct
      let loc = loc
    end) in
    (* we need to name the arguments of the constructors only if they are the same *)
    if c0 = c1 then
      let var0, pat0 = constructor_pattern ~loc (c0, arg0) in
      let var1, pat1 = constructor_pattern ~loc (c1, args1) in
      (ppat_tuple [ pat0; pat1 ], var0, var1)
    else
      let pat0 = constructor_any_pattern ~loc (c0, arg0) in
      let pat1 = constructor_any_pattern ~loc (c1, args1) in
      (ppat_tuple [ pat0; pat1 ], [], [])

  let rhs ~loc ~combinator ~default ~operators var0 var1 =
    if List.length var0 = 0 then default
    else fold ~loc ~combinator ~operators var0 var1

  let case ~loc ~combinator ~default ~operators c0 c1 =
    let lhs, var0, var1 = lhs ~loc c0 c1 in
    let rhs = rhs ~loc ~combinator ~default ~operators var0 var1 in
    Ast_builder.Default.case ~lhs ~guard:None ~rhs

  let same_constructor_cases ~loc ~combinator ~default =
    List.map2 (fun operators c -> case ~loc ~combinator ~default ~operators c c)

  let catch_all ~loc ~default =
    let open Ast_builder.Make (struct
      let loc = loc
    end) in
    [ case ~lhs:(ppat_tuple [ ppat_any; ppat_any ]) ~guard:None ~rhs:default ]

  let variant ~loc ~cases =
    let open Ast_builder.Make (struct
      let loc = loc
    end) in
    let a, b = (gen_symbol ~prefix:"__a" (), gen_symbol ~prefix:"__b" ()) in
    let case_split = pexp_match (pexp_tuple [ evar a; evar b ]) cases in
    [%expr fun [%p pvar a] [%p pvar b] -> [%e case_split]]
end

module Comparison = struct
  let if_then_else ~loc cmp elt_a elt_b next =
    let open Ast_builder.Make (struct
      let loc = loc
    end) in
    let res = gen_symbol ~prefix:"__res" () in
    [%expr
      let [%p pvar res] = [%e cmp] [%e evar elt_a] [%e evar elt_b] in
      if [%e evar res] <> 0 then [%e evar res] else [%e next]]

  module Variant = struct
    (** In this module we define some auxiliary functions for generating
        comparison about variants *)

    (* specific to comparison *)
    let inf_cases ~loc constructors =
      (* check a.(i) against b.(j) for all i < j *)
      let a = Array.of_list constructors in
      let b = Array.of_list constructors in
      let stop = Array.length a in
      let rec loop i j =
        if i = stop then []
        else if j = stop then loop (succ i) (i + 2)
        else
          Common.case ~loc ~combinator:if_then_else ~default:[%expr -1]
            ~operators:[] a.(i) b.(j)
          :: loop i (succ j)
      in
      loop 0 1
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
    | "seq" ->
        Error (W.Unsupported_comparison "seq")
        (* XXX How seq are implemented ? *)
    | "bag" -> Error (W.Unsupported_comparison "bag")
    | "set" -> Error (W.Unsupported_comparison "set")
    | _ -> assert false

  and synonyms ~loc (constructor : Translated.type_) args =
    Common.synonyms ~loc ~derive ~core constructor args

  and variant ~loc constructors =
    Common.result_of_list
      (fun (_, c) -> Common.constructor derive c)
      constructors
    |> Result.map (fun operators ->
           let cases =
             Common.same_constructor_cases ~loc ~combinator:if_then_else
               ~default:[%expr 0] operators constructors
             @ Variant.inf_cases ~loc constructors
             @ Common.catch_all ~loc ~default:[%expr 1]
           in
           Common.variant ~loc ~cases)

  and tuple ~loc = Common.tuple ~loc ~derive ~combinator:if_then_else
  and record ~loc = Common.record ~loc ~derive ~combinator:if_then_else

  and derive (t : Translated.type_) =
    let loc = t.loc in
    match t.kind with
    | Translated.Abstract -> Error (W.Unsupported_comparison "abstract")
    | Translated.Alpha -> Error (W.Unsupported_comparison "polymorphic")
    | Translated.Core args -> core ~loc t.name args
    | Translated.Synonyms (c, a) -> synonyms ~loc c a
    | Translated.Variant cstrs -> variant ~loc cstrs
    | Translated.Record fields -> record ~loc fields
    | Translated.Tuple types -> tuple ~loc types
end

module Equality = struct
  let conjunction ~loc eq elt_a elt_b next =
    let open Ast_builder.Make (struct
      let loc = loc
    end) in
    [%expr [%e eq] [%e evar elt_a] [%e evar elt_b] && [%e next]]

  let rec core ~loc name args =
    let open Ast_builder.Make (struct
      let loc = loc
    end) in
    match name with
    | "unit" | "bool" | "int" | "char" | "string" | "float" | "integer" ->
        Ok [%expr ( = )]
    | "option" ->
        let build eq = [%expr Option.equal [%e eq]] in
        Result.map build (derive (List.hd args))
    | "list" ->
        let build eq =
          [%expr
            fun a b ->
              try List.for_all2 [%e eq] a b with Invalid_argument _ -> false]
        in
        Result.map build (derive (List.hd args))
    | "array" ->
        let build eq = [%expr Ortac_runtime.Array.equal [%e eq]] in
        Result.map build (derive (List.hd args))
    | "ref" ->
        let a = gen_symbol ~prefix:"__a" () in
        let b = gen_symbol ~prefix:"__b" () in
        let build eq =
          [%expr
            fun [%p pvar a] [%p pvar b] -> [%e eq] ![%e evar a] ![%e evar b]]
        in
        Result.map build (derive (List.hd args))
    | "seq" -> Error (W.Unsupported_equality "seq")
    | "bag" -> Error (W.Unsupported_equality "bag")
    | "set" -> Error (W.Unsupported_equality "set")
    | _ -> assert false

  and synonyms ~loc (constructor : Translated.type_) args =
    Common.synonyms ~loc ~derive ~core constructor args

  and variant ~loc constructors =
    Common.result_of_list
      (fun (_, c) -> Common.constructor derive c)
      constructors
    |> Result.map (fun operators ->
           let cases =
             Common.same_constructor_cases ~loc ~combinator:conjunction
               ~default:[%expr true] operators constructors
             @ Common.catch_all ~loc ~default:[%expr false]
           in
           Common.variant ~loc ~cases)

  and record ~loc fields =
    Common.record ~loc ~derive ~combinator:conjunction fields

  and tuple ~loc = Common.tuple ~loc ~derive ~combinator:conjunction

  and derive (t : Translated.type_) =
    let loc = t.loc in
    match t.kind with
    | Translated.Abstract -> Error (W.Unsupported_equality "abstract")
    | Translated.Alpha -> Error (W.Unsupported_equality "polymorphic")
    | Translated.Core args -> core ~loc t.name args
    | Translated.Synonyms (c, a) -> synonyms ~loc c a
    | Translated.Variant cstrs -> variant ~loc cstrs
    | Translated.Record fields -> record ~loc fields
    | Translated.Tuple types -> tuple ~loc types
end
