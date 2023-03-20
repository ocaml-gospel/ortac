let main path init sut =
  match Config.init path init sut with
  | Ok config ->
      let open Gospel.Identifier.Ident in
      let msg =
        "This will generate qcheck-stm tests using `"
        ^ config.sut.ts_ident.id_str
        ^ "' as principal type and `"
        ^ config.init.id_str
        ^ "' as init function."
      in
      print_endline msg
  | Error err -> print_endline err

open Cmdliner

module Plugin : sig
  val cmd : unit Cmd.t
end = struct
  let info =
    Cmd.info "qcheck-stm"
      ~doc:"Generate QCheck-stm test file according to Gospel specifications."

  let sut =
    Arg.(
      required
      & pos 1 (some string) None
      & info [] ~doc:"Build the qcheck-stm tests with SUT." ~docv:"SUT")

  let init =
    Arg.(
      required
      & pos 2 (some string) None
      & info []
          ~doc:
            "Build the qcheck-stm tests using INIT function to initialize the \
             system under test."
          ~docv:"INIT")

  let term =
    let open Registration in
    Term.(const main $ ocaml_file $ init $ sut)

  let cmd = Cmd.v info term
end

let () = Registration.register Plugin.cmd
