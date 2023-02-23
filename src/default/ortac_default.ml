let signature ~runtime ~module_name namespace s =
  let open Ortac_core in
  let context = Context.init module_name namespace in
  let translated = Translate.signature ~context s in
  Report.emit_warnings Fmt.stderr translated;
  Generate.structure runtime translated

let generate path output =
  let module_name = Ortac_core.Utils.module_name_of_path path in
  let output = Format.formatter_of_out_channel output in
  Gospel.Parser_frontend.parse_ocaml_gospel path
  |> Ortac_core.Utils.type_check [] path
  |> fun (env, sigs) ->
  assert (List.length env = 1);
  signature ~runtime:"Ortac_runtime" ~module_name (List.hd env)
    sigs
  |> Fmt.pf output "%a@." Ppxlib_ast.Pprintast.structure
