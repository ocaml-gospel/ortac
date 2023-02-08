let get_channel = function None -> stdout | Some path -> open_out path

open Cmdliner

let setup_log =
  let init style_renderer = Fmt_tty.setup_std_outputs ?style_renderer () in
  Term.(const init $ Fmt_cli.style_renderer ())

let output_file =
  let parse s =
    match Sys.is_directory s with
    | true -> Error (`Msg (Fmt.str "Error: `%s' is a directory" s))
    | false | (exception Sys_error _) -> Ok (Some s)
  in
  Arg.(
    value
    & opt (conv ~docv:"OUTPUT" (parse, Fmt.(option string))) None
    & info [ "o"; "output" ] ~absent:"stdout" ~docv:"OUTPUT"
        ~doc:
          "Prints the generated code in OUTPUT. OUTPUT shouldn't be the name \
           of a directory. Creates the file if it does not exists.")

let ocaml_file =
  let parse s =
    match Sys.file_exists s with
    | true ->
        if Sys.is_directory s || Filename.extension s <> ".mli" then
          `Error (Fmt.str "Error: `%s' is not an OCaml interface file" s)
        else `Ok s
    | false -> `Error (Fmt.str "Error: `%s' not found" s)
  in
  Arg.(required & pos 0 (some (parse, Fmt.string)) None & info [] ~docv:"FILE")

module Default : sig
  val cmd : unit Cmd.t
end = struct
  let main input output () =
    let channel = get_channel output in
    try Ortac_default.generate input channel
    with Gospel.Warnings.Error e ->
      Fmt.epr "%a@." Gospel.Warnings.pp e;
      exit 1

  let info = Cmd.info "default" ~doc:"Simple assertion checking wrapper."
  let term = Term.(const main $ ocaml_file $ output_file $ setup_log)
  let cmd = Cmd.v info term
end

module Monolith : sig
  val cmd : unit Cmd.t
end = struct
  let main input output () =
    let channel = get_channel output in
    try Ortac_monolith.generate input channel
    with Gospel.Warnings.Error e ->
      Fmt.epr "%a@." Gospel.Warnings.pp e;
      exit 1

  let info =
    Cmd.info "monolith"
      ~doc:"Generates Monolith test file according to Gospel specifications."

  let term = Term.(const main $ ocaml_file $ output_file $ setup_log)
  let cmd = Cmd.v info term
end

let group =
  let doc = "Run ORTAC." in
  let version = "ortac version %%VERSION%%" in
  let info = Cmd.info "ortac" ~doc ~version in
  Cmd.group info [ Default.cmd; Monolith.cmd ]

let () = Stdlib.exit (Cmd.eval group)
