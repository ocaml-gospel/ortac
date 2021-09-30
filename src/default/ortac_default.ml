module M : Ortac_core.Frontend.S = struct
  let prelude = []
end

module G = Ortac_core.Ortac.Make (M)

let generate path output =
  let module_name = Ortac_core.Utils.module_name_of_path path in
  let output = Format.formatter_of_out_channel output in
  Gospel.Parser_frontend.parse_ocaml_gospel path
  |> Ortac_core.Utils.type_check [] path
  |> fun (env, sigs) ->
  G.signature module_name env sigs |> Ppxlib_ast.Pprintast.structure output;
  Format.pp_print_flush output ();
  Ortac_core.Warnings.report ()
