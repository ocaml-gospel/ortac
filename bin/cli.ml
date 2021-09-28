type frontend = Default | Monolith

let frontend_printer f = function
  | Default -> Format.pp_print_string f "Default"
  | Monolith -> Format.pp_print_string f "Monolith"

let frontend_parser = function
  | "default" -> Ok Default
  | "monolith" -> Ok Monolith
  | s -> Error (`Msg (Printf.sprintf "Error: `%s' is not a valid argument" s))

let main frontend path () =
  match frontend with
  | Default -> Ortac_default.generate path
  | Monolith -> Ortac_monolith.generate path

open Cmdliner

let setup_log =
  let init style_renderer = Fmt_tty.setup_std_outputs ?style_renderer () in
  Term.(const init $ Fmt_cli.style_renderer ())

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

let frontend =
  Arg.(
    value
    & opt (conv ~docv:"FRONTEND" (frontend_parser, frontend_printer)) Default
    & info [ "f"; "frontend" ] ~docv:"FRONTEND")

let cmd =
  let doc = "Run ORTAC." in
  let version = "ortac version %%VERSION%%" in
  ( Term.(const main $ frontend $ ocaml_file $ setup_log),
    Term.info "ortac" ~version ~doc )

let () = Term.(exit @@ eval cmd)
