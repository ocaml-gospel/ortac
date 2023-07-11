open Gospel

module Mutability = struct
  let max m n =
    let open Ir in
    match (m, n) with
    | Mutable, _ | _, Mutable -> Mutable
    | Dependant f, _ | _, Dependant f -> Dependant f
    | Unknown, _ | _, Unknown -> Unknown
    | Immutable, Immutable -> Immutable

  let min_mut = Ir.Immutable

  let tysymbol ~ir (ts : Ttypes.tysymbol) =
    (* To determine the mutability of a `tysymbol`, we look in the ir. *)
    match Ir.get_type ts ir with
    | None ->
        (* If the ir doesn't know about this `tysymbol`, the mutability is `Unknown`.
           Note that of the type is parametric we wait for the instantiation to deliver the
           information *)
        if List.length ts.ts_args > 0 then Ir.Dependant (fun _ -> Unknown)
        else Ir.Unknown
    | Some t -> t.mutable_

  let alpha (ty : Ttypes.ty) =
    match ty.ty_node with Tyvar _ -> true | _ -> false

  let rec ty ~ir (t : Ttypes.ty) =
    match t.ty_node with
    | Tyvar _ ->
        (* A `Tyvar` is an alpha *)
        Ir.Unknown
    | Tyapp (ts, tyl) when Ttypes.is_ts_tuple ts ->
        (* XXX not sure about that decision... *)
        (* In the presence of a tuple there are three cases
           1. There is at least an alpha in its element but another element is already known to be mutable
           2. There is at least an alpha but none element is known to be mutable
           3. All the elements are known types *)
        if List.exists alpha tyl then
          max
            (List.map (ty ~ir) tyl |> List.fold_left max min_mut)
            (Ir.Dependant (fun tyl -> List.fold_right max tyl min_mut))
        else List.map (ty ~ir) tyl |> List.fold_left max min_mut
    | Tyapp (ts, tyl) when List.length tyl = 0 ->
        (* If `tyl` is empty, we just look at `ts`. The mutability can't be `Dependant` *)
        tysymbol ~ir ts
    | Tyapp (ts, tyl) -> (
        (* It the list is not empty, that means that `ts` is a parametric type and
           its mutability is `Dependant` *)
        match tysymbol ~ir ts with
        | Ir.Dependant f as dep ->
            (* It there is still an alpha in the parameters, the mutability is still `Dependant`,
               otherwise, we apply the function embebed in the `Dependant` to the parameters *)
            if List.exists alpha tyl then dep else f (List.map (ty ~ir) tyl)
        | _ -> assert false)

  let lsymbol ~ir (ls : Symbols.lsymbol) =
    (* To determine the mutability of a `lsymbol` we look at its `ls_value`
       which is a `Ttypes.ty option`.
       If there is none, the mutability is unknown *)
    Option.fold ~none:Ir.Unknown ~some:(ty ~ir) ls.ls_value

  let constructor_declaration ~ir (cd : Tast.constructor_decl) =
    (* The mutability of a constructor is the max od the mutability of its argument *)
    List.map (ty ~ir) cd.cd_cs.ls_args |> List.fold_left max min_mut

  let field_declaration ~ir (ld : Symbols.lsymbol Tast.label_declaration) =
    (* A record field is mutable if it is annotated as mutable, if not we look
       at the lsymbol it contains i.e. the field itself. *)
    match ld.ld_mut with
    | Tast.Mutable -> Ir.Mutable
    | Tast.Immutable -> lsymbol ~ir ld.ld_field

  let type_declaration ~ir (td : Tast.type_declaration) =
    (* To determine the mutability of a type declaration, we check
       whether it is an alias. *)
    match td.td_ts.ts_alias with
    | Some alias ->
        (* An alias it a Ttypes.ty, e.g. `type t = int` *)
        ty ~ir alias
    | None -> (
        match td.td_kind with
        (* We don't have any information on an abstract type *)
        | Pty_abstract -> Ir.Unknown
        (* The mutability of a variant is the max of the mutability of its contructor *)
        | Pty_variant cdl ->
            List.map (constructor_declaration ~ir) cdl
            |> List.fold_left max min_mut
        | Pty_record rd ->
            (* The mutability of a record is the max of the mutability of its fields *)
            List.map (field_declaration ~ir) rd.rd_ldl
            |> List.fold_left max min_mut)

  let mutable_model ~ir (ty_fields : (Symbols.lsymbol * bool) list) =
    List.map
      (* if a model is annotated as mutable, it is mutable, if not we look at
         the type of the model *)
        (fun (ls, b) -> if b then Ir.Mutable else lsymbol ~ir ls)
      ty_fields
    (* the mutability of a type is here the max of the mutability of its models *)
    |> List.fold_left max min_mut

  let type_spec ~ir (spec : Tast.type_spec) =
    (* To determine the mutability of a type according to its specification
       we check whether it is annotated with an ephemeral, and if not we
       look at its models *)
    if spec.ty_ephemeral then Ir.Mutable else mutable_model ~ir spec.ty_fields
end
