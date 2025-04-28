module Ir = Ortac_wrapper__Ir
module Utils = Ortac_core__Utils

let translate path =
  let module_name = Utils.module_name_of_path path in
  Gospel.Parser_frontend.parse_ocaml_gospel path |> Utils.type_check [] path
  |> fun (env, sigs) ->
  assert (List.length env = 1);
  let context = Ortac_core.Context.init module_name (List.hd env) in
  Ortac_wrapper__Ir_of_gospel.signature ~context sigs

let is_pure = function Ir.Value v -> v.pure | _ -> assert false
let type_name = function Ir.Type t -> t.name | _ -> assert false
let val_name = function Ir.Value v -> v.name | _ -> assert false

let is_val (t : Ir.structure_item) =
  match t with Ir.Value _ -> true | _ -> false

let val_pure () =
  let translations = translate "./translation/pure.mli" in
  Ir.iter_translation
    ~f:(fun v ->
      Alcotest.(check bool) (Fmt.str "%s is pure" (val_name v)) true (is_pure v))
    translations

let suite = ("Translation", [ ("value purity", `Quick, val_pure) ])
