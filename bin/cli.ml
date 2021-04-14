type generator = Default | Monolith

let backend_printer f = function
  | Default -> Format.pp_print_string f "Default"
  | Monolith -> Format.pp_print_string f "Monolith"

let main = function
  | Default -> Gospel_rtac.Backend.generate
  | Monolith -> Gospel_rtac_monolith.Backend.generate

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

let backend =
  let parse = function
    | "default" -> Ok Default
    | "monolith" -> Ok Monolith
    | s -> Error (`Msg (Printf.sprintf "Error: `%s' is not a valid argument" s))
  in
  Arg.(
    value
    & opt (conv ~docv:"BACKEND" (parse, backend_printer)) Default
    & info [ "g"; "backend" ] ~docv:"BACKEND")

let cmd =
  let doc = "Run GOSPEL-RTAC." in
  (Term.(const main $ backend $ ocaml_file), Term.info "gospel-rtac" ~doc)

let () = Term.(exit @@ eval cmd)
