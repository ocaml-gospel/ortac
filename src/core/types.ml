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

module Derive = struct
  module W = Warnings
  open Ppxlib

  let rec type_depth (t : Translated.type_) =
    (* XXX could be more precise:
       - taking into account the number of parameters
       - distinguishing between lists and option for example *)
    match t.args with
    | [] -> 0
    | xs -> List.map type_depth xs |> List.fold_left max 0

  let depth_treshold = 2
  let complex t = type_depth t > depth_treshold

  type kind = Equality | Comparison

  let base ~loc = function
    | Equality -> [%expr fun a b -> a = b]
    | Comparison -> [%expr compare]

  let option ~loc = function
    | Equality -> fun eq -> [%expr Option.equal [%e eq]]
    | Comparison -> fun cmp -> [%expr Option.compare [%e cmp]]

  let list ~loc complex kind =
    if complex then
      match kind with
      | Equality ->
          fun eq ->
            [%expr
              fun a b -> List.compare_lengths a b && List.equal [%e eq] a b]
      | Comparison ->
          fun cmp ->
            [%expr
              fun a b -> List.compare_lengths a b && List.compare [%e cmp] a b]
    else
      match kind with
      | Equality -> fun eq -> [%expr List.equal [%e eq]]
      | Comparison -> fun cmp -> [%expr List.compare [%e cmp]]

  let array ~loc kind =
    match kind with
    | Equality ->
        fun eq ->
          (* since 4.11.0, stdlib already check length before comparing elements *)
          [%expr
            fun a b ->
              try Array.for_all2 [%e eq] ab with Invalid_argument -> false]
    | Comparison ->
        (* XXX Array.find_map is in stdlib since 4.13.0 *)
        (* We use the lexicographic order according to the comparison function
           derived from the type of the elements *)
        fun cmp ->
         [%expr
           fun a b ->
             let l = compare (Array.length a) (Array.length b) in
             if l = 0 then
               let ar = Array.combine a b in
               let cmp (x, y) =
                 if [%e cmp] x y = 0 then None else [%e cmp] x y
               in
               match Array.find_map cmp ar with None -> 0 | Some i -> i
             else l]

  let inefficient_bag_equality ~loc eq =
    (* If we don't have a comparison function, but only an equality function,
       we use an inefficient equality.
         Two bags a and b are equals iff:
         - for all elements in a there are the same occurences in b
         - and no element of b is not an element of a (no need to check number of occurrences here) *)
    [%expr
      let occ a x =
        Array.fold_right (fun y n -> if [%e eq] x y then succ n else n) a 0
      in
      fun a b ->
        Array.length a = Array.length b
        && Array.for_all (fun x -> occ a x = occ b x) a
        && Array.for_all (fun x -> Array.exists (fun y -> [%e eq] x y) a) b]

  let efficient_bag_equality ~loc eq cmp =
    [%expr
      fun a b ->
        let a' = Array.copy a in
        let b' = Array.copy b in
        Array.sort [%e cmp] a';
        Array.sort [%e cmp] b';
        try Array.for_all2 [%e eq] a' b' with Invalid_argument -> false]

  let inefficient_bag_comparison ~loc eq =
    [%expr
      let occ a x =
        Array.fold_right (fun y n -> if [%e eq] x y then succ n else n) a 0
      in
      let inclusion a b =
        (* a is include in b *)
        Array.for_all (fun x -> occ a x <= occ b x) a
      in
      fun a b ->
        let al, bl = (Array.length a, Array.length b) in
        if al = bl then if inclusion a b && inclusion b a then Some 0 else None
        else if al < bl then if inclusion a b then Some (-1) else None
        else if al > bl then if incluion b a then Some 1 else None]

  let efficient_bag_comparison ~loc eq cmp =
    [%expr
      let inclusion a b =
        (* a is include in b *)
        let stop = Array.length a in
        let wrong = Array.length b in
        let rec loop i j =
          (* XXX could be better:
             - last case find next element in second bag
             - check comparison *)
          if i = stop then true
          else if j = wrong then false
          else if [%e eq] a.(i) a.(j) then loop (succ i) (succ j)
          else loop i (succ j)
        in
        loop 0 0
      in
      fun a b ->
        let a' = Array.copy a in
        let b' = Array.copy b in
        Array.sort [%e cmp] a';
        Array.sort [%e cmp] b';
        let al, bl = (Array.length a, Array.length b) in
        if al = bl then if Array.for_all2 [%e eq] a' b' then Some 0 else None
        else if al < bl then if inclusion a b then Some (-1) else None
        else if al > bl then if incluion b a then Some 1 else None]

  let bag ~loc eq cmp = function
    (* XXX this is correct only when bags are implemented as arrays *)
    | Equality -> (
        match cmp with
        | Error _ -> inefficient_bag_equality ~loc eq
        | Ok cmp -> efficient_bag_equality ~loc eq cmp)
    | Comparison -> (
        (* Comparison between bags is meaningful w.r.t the inclusion relation.
           Hence, it is *not* decidable.
           So, we decide to put the result in an option. *)
        match cmp with
        | Error _ -> inefficient_bag_comparison ~loc eq
        | Ok cmp -> efficient_bag_comparison ~loc eq cmp)

  let inefficient_set_equality ~loc eq =
    [%expr
      fun a b ->
        Array.for_all (fun x -> Array.exists (fun i -> [%e eq] i x) b) a
        && Array.for_all (fun x -> Array.exists (fun i -> [%e eq] i x) a) b]

  let efficient_set_equality ~loc eq cmp =
    [%expr
      fun a b ->
        let a' = Array.copy a in
        let b' = Array.copy b in
        Array.sort [%e cmp] a';
        Array.sort [%e cmp] b';
        let next i ar =
          let rec next x i ar =
            if i = Array.length ar then None
            else if [%e cmp] x ar.(i) < 0 then Some i
            else next x (succ i) ar
          in
          next ar.(i) i ar
        in
        let rec loop i j =
          if [%e eq] a'.(i) b'.(j) then
            match (next i a', next j b') with
            | None, None -> true
            | Some i, Some j -> loop i j
            | _, _ -> false
        in
        loop 0 0]

  let inefficient_set_comparison ~loc eq =
    [%expr
      fun a b ->
        let mem x = Array.exists (fun y -> [%e eq] x y) in
        let a_in_b = Array.for_all (fun x -> mem x b) a in
        let b_in_a = Array.for_all (fun x -> mem x a) b in
        match (a_in_b, b_in_a) with
        | true, true -> Some 0
        | true, false -> Some (-1)
        | false, true -> Some 1
        | false, false -> None]

  let efficient_set_comparison ~loc cmp =
    [%expr
      let next i ar =
        let stop = Array.length ar in
        let rec next x i ar =
          if i = stop then None
          else if [%e cmp] x ar.(i) < 0 then Some i
          else next x (succ i) ar
        in
        next ar.(i) i ar
      in
      fun a b ->
        let a' = Array.copy a in
        let b' = Array.copy b in
        Array.sort [%e cmp] a';
        Array.sort [%e cmp];
        let al = Array.length a' in
        let bl = Array.length b' in
        let rec loop res i j =
          (* process both sets one element at a time *)
          (* invariant:
             - a'.(i) has not been seen in the processed part of b'
             - b'.(j) has not been seen in the processed part of a' *)
          let a_cmp_b = [%e cmp] a'.(i) b'.(j) in
          let next_i = next i a' in
          let next_j = next j b' in
          match (a_cmp_b, next_i, next_j) with
          | 0, None, None -> Some res
          | 0, Some i, None -> if res >= 0 then Some 1 else None
          | 0, None, Some j -> if res <= 0 then Some (-1) else None
          | 0, Some i, Some j -> loop res i j
          | 1, None, None -> None
          | 1, Some i, None -> None
          | 1, None, Some j -> if res = -1 then loop res i j else None
          | 1, Some _, Some j -> if res <= 0 then loop res i j else None
          | -1, None, None -> None
          | -1, Some i, None -> if res = 1 then loop res i j else None
          | -1, None, Some j -> None
          | -1, Some i, Some _ -> if res <= 0 then loop res i j else None
        in
        loop 0 0 0]

  let set ~loc eq cmp = function
    | Equality -> (
        match cmp with
        | Error _ -> inefficient_set_equality ~loc eq
        | Ok cmp -> efficient_set_equality ~loc eq cmp)
    | Comparison -> (
        match cmp with
        | Error _ -> inefficient_set_comparison ~loc eq
        | Ok cmp -> efficient_set_comparison ~loc cmp)

  let rec derive kind (t : Translated.type_) =
    let loc = t.loc in
    match t.name with
    | "unit" | "int" | "integer" | "char" | "string" | "bool" | "float" ->
        Ok (base ~loc kind)
    | "option" -> (
        match derive kind (List.hd t.args) with
        | Error e -> Error e
        | Ok fct -> Ok ((option ~loc kind) fct))
    | "list" -> (
        let arg = List.hd t.args in
        match derive kind arg with
        | Error e -> Error e
        | Ok fct -> Ok ((list ~loc (complex arg) kind) fct))
    | "array" -> (
        let arg = List.hd t.args in
        match derive kind arg with
        | Error e -> Error e
        | Ok fct -> Ok ((array ~loc kind) fct))
    | "bag" -> (
        let arg = List.hd t.args in
        (* for both Equality and Comparison, the minimum we need is the equality function *)
        match derive Equality arg with
        (* XXX error message may not be completely accurate... *)
        | Error e -> Error e
        | Ok eq -> Ok (bag ~loc eq (derive Comparison arg) kind))
    | "set" -> (
        let arg = List.hd t.args in
        match derive Equality arg with
        (* XXX error message may not be completely accurate... *)
        | Error e -> Error e
        | Ok eq -> Ok (set ~loc eq (derive Comparison arg) kind))
    | "seq" ->
        (* XXX how are implemented sequences ? *)
        Error (W.Unsupported_equality "seq", loc)
    | ty -> Error (W.Unsupported_equality ty, loc)
end

module Equality = struct
  let derive = Derive.derive Derive.Equality
end

module Comparison = struct
  let derive = Derive.derive Derive.Comparison
end
