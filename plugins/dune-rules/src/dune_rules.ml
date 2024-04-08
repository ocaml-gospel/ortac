open Cmdliner

module Plugin : sig
  val cmd : unit Cmd.t
end = struct
  module Qcheck_stm : sig
    val cmd : unit Cmd.t
  end = struct
    let info =
      Cmd.info "qcheck-stm"
        ~doc:"Generate Dune rules for the qcheck-stm plugin."

    let interface_file =
      Arg.(
        required
        & pos 0 (some string) None
        & info [] ~doc:"Interface file containing Gospel specifications"
            ~docv:"INTERFACE")

    let config_file =
      Arg.(
        required
        & pos 1 (some string) None
        & info [] ~doc:"Configuration file for Ortac/QCheckSTM" ~docv:"CONFIG")

    let ocaml_output =
      Arg.(
        required
        & pos 2 (some string) None
        & info [] ~doc:"Filename for the generated tests" ~docv:"OCAML_OUTPUT")

    let library =
      Arg.(
        value
        & opt (some string) None
        & info [ "l"; "library" ]
            ~doc:"Name of the library the module under test belongs to."
            ~absent:"INTERFACE without the file extension" ~docv:"LIBRARY")

    let package_name =
      Arg.(
        value
        & opt (some string) None
        & info [ "p"; "package" ] ~doc:"Package name." ~docv:"PACKAGE")

    let with_stdout_to =
      Arg.(
        value
        & opt (some string) None
        & info [ "w"; "with-stdout-to" ]
            ~doc:
              "Filename for the generated dune rules. For use on the command \
               line."
            ~docv:"DUNE_OUTPUT")

    let main interface_file config_file ocaml_output library package_name
        dune_output =
      let open Qcheck_stm in
      let config =
        {
          interface_file;
          config_file;
          ocaml_output;
          library;
          package_name;
          dune_output;
        }
      in
      let ppf = Registration.get_out_formatter dune_output in
      Qcheck_stm.gen_dune_rules ppf config

    let term =
      Term.(
        const main
        $ interface_file
        $ config_file
        $ ocaml_output
        $ library
        $ package_name
        $ with_stdout_to)

    let cmd = Cmd.v info term
  end

  let cmd =
    let info = Cmd.info "dune" ~doc:"Generate Dune rule for ortac plugins." in
    Cmd.group info [ Qcheck_stm.cmd ]
end

(* let () = Stdlib.exit (Cmd.eval Plugin.cmd) *)
let () = Registration.register Plugin.cmd
