let generate path output =
  let module_name = Ortac_core.Utils.module_name_of_path path in
  let output = Format.formatter_of_out_channel output in
  Gospel.Parser_frontend.parse_ocaml_gospel path
  |> Ortac_core.Utils.type_check [] path
  |> fun (env, sigs) ->
  assert (List.length env = 1);
  Ortac_core.Ortac.signature ~runtime:"Ortac_runtime" ~module_name (List.hd env)
    sigs
  |> Fmt.pf output "%a@." Ppxlib_ast.Pprintast.structure
