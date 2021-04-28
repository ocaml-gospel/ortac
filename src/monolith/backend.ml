open Ppxlib

module M : Ortac_core.Backend.S = struct
  let prelude =
    let loc = Location.none in
    [ [%stri open Ortac_runtime] ]
end

module G = Ortac_core.Ortac.Make (M)

let generate path =
  let module_name = Ortac_core.Utils.module_name_of_path path in
  Gospel.Parser_frontend.parse_ocaml_gospel path
  |> Ortac_core.Utils.type_check [] path
  |> G.signature module_name
  |> Ppxlib_ast.Pprintast.structure Fmt.stdout
