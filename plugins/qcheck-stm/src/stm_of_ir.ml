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

let subst inst ty =
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

let cmd_constructor config value =
  let rec aux ty : Ppxlib.core_type list =
    match ty.ptyp_desc with
    | Ptyp_arrow (_, l, r) ->
        if Cfg.is_sut config l then aux r
        else
          let x = subst value.inst l and xs = aux r in
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
