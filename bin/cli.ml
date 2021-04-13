open Ppxlib
open Gospel

let module_name_of_path p =
  let filename = Filename.basename p in
  String.index filename '.' |> String.sub filename 0 |> String.capitalize_ascii

let type_check load_path name sigs =
  let md = Tmodule.init_muc name in
  let penv =
    module_name_of_path name |> Utils.Sstr.singleton |> Typing.penv load_path
  in
  List.fold_left (Typing.type_sig_item penv) md sigs |> Tmodule.wrap_up_muc
  |> fun file -> file.fl_sigs

let main generator path =
  let module_name = module_name_of_path path in
  Parser_frontend.parse_ocaml_gospel path
  |> type_check [] path
  |> Gospel_rtac.main generator module_name
  |> Pprintast.structure Fmt.stdout

open Cmdliner

let ocaml_file =
  let parse s =
    match Sys.file_exists s with
    | true ->
        if Sys.is_directory s (* || Filename.extension s <> ".mli" *) then
          `Error (Printf.sprintf "Error: `%s' is not an OCaml interface file" s)
        else `Ok s
    | false -> `Error (Printf.sprintf "Error: `%s' not found" s)
  in
  Arg.(
    required
    & pos 0 (some (parse, Format.pp_print_string)) None
    & info [] ~docv:"FILE")

let generator =
  let doc = "" in
  Arg.(
    value & opt string "default"
    & info [ "g"; "generator" ] ~docv:"GENERATOR" ~doc)

let cmd =
  let doc = "Run GOSPEL-RTAC." in
  (Term.(const main $ generator $ ocaml_file), Term.info "gospel-rtac" ~doc)

let () = Term.(exit @@ eval cmd)
