module M : Ortac_core.Frontend.S = struct
  let prelude = []
end

module G = Ortac_core.Ortac.Make (M)

let run f path =
  let module_name = Ortac_core.Utils.module_name_of_path path in
  Gospel.Parser_frontend.parse_ocaml_gospel path
  |> Ortac_core.Utils.type_check [] path
  |> fun (env, sigs) ->
  assert (List.length env = 1);
  f module_name (List.hd env) sigs

let generate path output =
  let output = Format.formatter_of_out_channel output in
  run G.signature path |> Fmt.pf output "%a@." Ppxlib_ast.Pprintast.structure

let report = run G.report
