open Gospel

module KEY = struct
  open Gospel.Ttypes

  (* keys has some structure so that we can recursively reconstruct the corresponding [Repr.t]
     from it given that the elements are already in the container *)
  type t = Alpha | Leaf of tysymbol | Node of (t * t list)

  let rec compare t0 t1 =
    let cmp = Gospel.Ttypes.Ts.compare in
    match (t0, t1) with
    | Leaf x, Leaf y -> cmp x y
    | Node (x, xs), Node (y, ys) -> (
        match compare x y with 0 -> List.compare compare xs ys | i -> i)
    | Leaf _, _ -> 1
    | _, _ -> -1
end

module M = Map.Make (KEY)

type key = KEY.t

let rec to_string key =
  let open KEY in
  match key with
  | Alpha -> "'a"
  | Leaf ts -> ts.ts_ident.id_str
  | Node (k, keys) -> String.concat " " (List.rev_map to_string (k :: keys))

(* The [kind] type constructor allows to discriminate between core and
    parametric types *)
(* [kind] is not necessarily the best name *)
(* it is possible, if necessary, to curry that
   For example if we want to store partial application of a type constructor *)
type 'a kind = Base of 'a | Forall of int * ('a list -> 'a)

let base a = Base a
let forall i f = Forall (i, f)

type expr = Ppxlib.expression kind
type info = { expr : expr; eq : string option; cmp : string option }

let info expr = { expr; eq = None; cmp = None }
let eq info = info.eq
(* let cmp info = info.cmp *)

let derive (info : info) =
  match info with
  | { expr = Base repr; eq = Some eq; cmp = _ } ->
      let open Ppxlib in
      let open Builder in
      let loc = Location.none in
      let n = gen_symbol ~prefix:"__repr" () in
      [
        [%stri let [%p pvar n] = [%e repr]];
        [%stri let [%p pvar eq] = Repr.(unstage (equal [%e evar n]))];
      ]
  | _ -> []

let derive_all map = M.bindings map |> List.concat_map (fun (_, i) -> derive i)

let toogle_eq info =
  if Option.is_none info.eq then
    { info with eq = Some (Ppxlib.gen_symbol ~prefix:"__equal_" ()) }
  else info

(* let toogle_cmp info = *)
(*   if Option.is_none info.cmp then *)
(*     { info with cmp = Some (Ppxlib.gen_symbol ~prefix:"__compare_" ()) } *)
(*   else info *)

type map = info M.t

let empty = M.empty
let get_info = M.find_opt
let get prj key map = Option.bind (get_info key map) prj
let get_equality = get eq
(* let get_cmp = get cmp *)

(** [expr_from_key map key] computes the [Ppxlib.expression] corresponding to
    the [Repr.t] of the type encoded by [key].

    The function fails for partial application of a type constructor. Which can
    be an undesirable behaviour...

    - [expr_from_key map (Leaf ts_int)] will return [\[%expr Repr.int\]]
    - [expr_from_key map (Node (ts_list, \[Leaf ts_int\]))] will return
      [\[%expr Repr.list Repr.int\]]
    - [expr_from_key map (Node (ts_option, \[\]))] will fail *)
let expr_from_key map key =
  let rec aux key =
    let open KEY in
    match key with
    | Alpha -> raise Exit
    | Leaf _ as k -> (
        match (M.find k map).expr with Base e -> e | Forall _ -> raise Exit)
    | Node (k, keys) -> (
        match (M.find k map).expr with
        | Base _ -> raise Exit
        | Forall (i, f) ->
            assert (List.length keys = i);
            List.map aux keys |> f)
  in
  try Some (aux key) with _ -> None

let add toogle key map =
  (match M.find_opt key map with
  | Some info -> Some (toogle info)
  | None ->
      let f expr = base expr |> info |> toogle in
      Option.map f (expr_from_key map key))
  |> Option.fold ~none:map ~some:(fun i -> M.add key i map)

(* let add_expr = add (fun x -> x) *)
let add_equality = add toogle_eq
(* let add_comparison = add toogle_cmp *)

let key (ty : Ttypes.ty) =
  let open Ttypes in
  let rec aux ty =
    match ty.ty_node with
    | Tyvar _ -> raise Exit
    | Tyapp (ts, []) -> KEY.Leaf ts
    | Tyapp (ts, tyl) -> Node (Leaf ts, List.map aux tyl)
  in
  try Some (aux ty) with Exit -> None

let rec raw_key ty =
  let open Ttypes in
  match ty.ty_node with
  | Tyvar _ -> KEY.Alpha
  | Tyapp (ts, []) -> KEY.Leaf ts
  | Tyapp (ts, tyl) -> Node (Leaf ts, List.map raw_key tyl)

let key_from_tysymbol (ts : Ttypes.tysymbol) = KEY.Leaf ts
let add_info = M.add

let rec traverse (term : Tterm.term) map =
  let open Tterm in
  match term.t_node with
  | Tvar _ -> map
  | Tconst _ -> map
  | Tapp (ls, [ lhs; rhs ]) when ls_equal ls ps_equ ->
      t_type lhs
      |> key
      |> Option.fold ~none:map ~some:(fun k ->
             add_equality k map |> traverse lhs |> traverse rhs)
  | Tapp (_, terms) -> List.fold_left (fun m t -> traverse t m) map terms
  | Tfield (term, _) -> traverse term map
  | Tif (cond, term0, term1) ->
      traverse cond map |> traverse term0 |> traverse term1
  | Tlet (_, term0, term1) -> traverse term0 map |> traverse term1
  | Tcase (pat, cases) ->
      let map = traverse pat map in
      List.fold_left (fun m (_, t) -> traverse t m) map cases
  | Tquant (_, _, term) -> traverse term map
  | Tbinop (_, left, right) -> traverse left map |> traverse right
  | Tnot term -> traverse term map
  | Told term -> traverse term map
  | Ttrue | Tfalse -> map

let fold_traverse = List.fold_right traverse
