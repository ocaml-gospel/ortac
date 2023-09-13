open Gospel
module W = Ortac_core.Warnings
module F = Failure
open Ppxlib
module Ident = Identifier.Ident
module VSet = Set.Make (Symbols.Vs)

let rec get_pattern_vars p =
  match p.Tterm.p_node with
  | Tterm.Pvar vs -> VSet.singleton vs
  | Tterm.Papp (_, args) ->
      List.fold_left
        (fun acc p -> VSet.union acc (get_pattern_vars p))
        VSet.empty args
  | Tterm.Por (p1, p2) -> VSet.union (get_pattern_vars p1) (get_pattern_vars p2)
  | Tterm.Pas (p, s) -> VSet.add s (get_pattern_vars p)
  | Tterm.Pwild | Tterm.Pinterval _ | Tterm.Pconst _ -> VSet.empty

let rec old_vars b t : Tterm.term =
  match t.Tterm.t_node with
  | Tvar x when VSet.mem x b -> t
  | Tvar _ -> { t with t_node = Told t }
  | Tconst _ | Ttrue | Tfalse -> t
  | Tapp (ls, tl) ->
      let tl = List.map (old_vars b) tl in
      let t_node = Tterm.Tapp (ls, tl) in
      { t with t_node }
  | Tfield (t, ls) ->
      let t = old_vars b t in
      let t_node = Tterm.Tfield (t, ls) in
      { t with t_node }
  | Tif (t1, t2, t3) ->
      let t1 = old_vars b t1 in
      let t2 = old_vars b t2 in
      let t3 = old_vars b t3 in
      let t_node = Tterm.Tif (t1, t2, t3) in
      { t with t_node }
  | Tterm.Tlet (vs, t1, t2) ->
      let t1 = old_vars b t1 in
      let t2 = old_vars (VSet.add vs b) t2 in
      let t_node = Tterm.Tlet (vs, t1, t2) in
      { t with t_node }
  | Tterm.Tcase (t, ptl) ->
      let t = old_vars b t in
      let ptl =
        List.map
          (fun (p, g, t) ->
            ( p,
              Option.map (old_vars b) g,
              old_vars (VSet.union (get_pattern_vars p) b) t ))
          ptl
      in
      let t_node = Tterm.Tcase (t, ptl) in
      { t with t_node }
  | Tquant (q, vsl, t) ->
      let t = old_vars (VSet.union (VSet.of_list vsl) b) t in
      let t_node = Tterm.Tquant (q, vsl, t) in
      { t with t_node }
  | Tterm.Tbinop (op, t1, t2) ->
      let t1 = old_vars b t1 in
      let t2 = old_vars b t2 in
      let t_node = Tterm.Tbinop (op, t1, t2) in
      { t with t_node }
  | Tterm.Tnot t ->
      let t = old_vars b t in
      let t_node = Tterm.Tnot t in
      { t with t_node }
  | Tterm.Told t -> old_vars b t

let rec old_down b t : Tterm.term =
  match t.Tterm.t_node with
  | Tvar _ | Tconst _ | Ttrue | Tfalse -> t
  | Tapp (ls, tl) ->
      let tl = List.map (old_down b) tl in
      let t_node = Tterm.Tapp (ls, tl) in
      { t with t_node }
  | Tfield (t, ls) ->
      let t = old_down b t in
      let t_node = Tterm.Tfield (t, ls) in
      { t with t_node }
  | Tif (t1, t2, t3) ->
      let t1 = old_down b t1 in
      let t2 = old_down b t2 in
      let t3 = old_down b t3 in
      let t_node = Tterm.Tif (t1, t2, t3) in
      { t with t_node }
  | Tterm.Tlet (vs, t1, t2) ->
      let t1 = old_down b t1 in
      let t2 = old_down (VSet.add vs b) t2 in
      let t_node = Tterm.Tlet (vs, t1, t2) in
      { t with t_node }
  | Tterm.Tcase (t, ptl) ->
      let t = old_down b t in
      let ptl =
        List.map
          (fun (p, g, t) ->
            ( p,
              Option.map (old_down b) g,
              old_down (VSet.union (get_pattern_vars p) b) t ))
          ptl
      in
      let t_node = Tterm.Tcase (t, ptl) in
      { t with t_node }
  | Tquant (q, vsl, t) ->
      let t = old_down (VSet.union (VSet.of_list vsl) b) t in
      let t_node = Tterm.Tquant (q, vsl, t) in
      { t with t_node }
  | Tterm.Tbinop (op, t1, t2) ->
      let t1 = old_down b t1 in
      let t2 = old_down b t2 in
      let t_node = Tterm.Tbinop (op, t1, t2) in
      { t with t_node }
  | Tterm.Tnot t ->
      let t = old_down b t in
      let t_node = Tterm.Tnot t in
      { t with t_node }
  | Tterm.Told t ->
      let t = old_vars b t in
      let t_node = Tterm.Told t in
      { t with t_node }

let fresh_var =
  let id = ref 0 in
  fun ty ->
    incr id;
    let str = Fmt.str "___ortac_copy_%d" !id in
    let preid = Identifier.Preid.create ~loc:Location.none str in
    Symbols.create_vsymbol preid (Option.value ~default:Ttypes.ty_bool ty)

let collect_old t =
  let rec aux acc t =
    match t.Tterm.t_node with
    | Tvar _ | Tconst _ | Ttrue | Tfalse -> ([], t)
    | Tapp (ls, tl) ->
        let acc, tl = List.fold_left_map aux acc tl in
        let t_node = Tterm.Tapp (ls, tl) in
        (acc, { t with t_node })
    | Tfield (t, ls) ->
        let acc, t = aux acc t in
        let t_node = Tterm.Tfield (t, ls) in
        (acc, { t with t_node })
    | Tif (t1, t2, t3) ->
        let acc, t1 = aux acc t1 in
        let acc, t2 = aux acc t2 in
        let acc, t3 = aux acc t3 in
        let t_node = Tterm.Tif (t1, t2, t3) in
        (acc, { t with t_node })
    | Tterm.Tlet (vs, t1, t2) ->
        let acc, t1 = aux acc t1 in
        let acc, t2 = aux acc t2 in
        let t_node = Tterm.Tlet (vs, t1, t2) in
        (acc, { t with t_node })
    | Tterm.Tcase (t, ptl) ->
        let acc, t = aux acc t in
        let acc, ptl =
          List.fold_left_map
            (fun acc (p, (g : Tterm.term option), t) ->
              let acc, g =
                Option.fold ~none:(acc, None)
                  ~some:(fun g ->
                    let acc, g = aux acc g in
                    (acc, Some g))
                  g
              in
              let acc, t = aux acc t in
              (acc, (p, g, t)))
            acc ptl
        in
        let t_node = Tterm.Tcase (t, ptl) in
        (acc, { t with t_node })
    | Tquant (q, vsl, t) ->
        let acc, t = aux acc t in
        let t_node = Tterm.Tquant (q, vsl, t) in
        (acc, { t with t_node })
    | Tterm.Tbinop (op, t1, t2) ->
        let acc, t1 = aux acc t1 in
        let acc, t2 = aux acc t2 in
        let t_node = Tterm.Tbinop (op, t1, t2) in
        (acc, { t with t_node })
    | Tterm.Tnot t ->
        let acc, t = aux acc t in
        let t_node = Tterm.Tnot t in
        (acc, { t with t_node })
    | Tterm.Told t ->
        let vs = fresh_var t.t_ty in
        let t_node = Tterm.Tvar vs in
        ((vs, t) :: acc, { t with t_node })
  in
  aux [] t
