open Cmdliner

let ocaml_output =
  Arg.(
    value
    & opt (some string) None
    & info [ "o"; "output" ]
        ~doc:
          "Filename for the generated tests. Useful for generating multiple \
           test files per module under test."
        ~absent:
          "concatenation of INTERFACE without the file extension and \
           \"_tests.ml\""
        ~docv:"OCAML_OUTPUT")

let interface_file =
  Arg.(
    required
    & pos 0 (some string) None
    & info [] ~doc:"Interface file containing Gospel specifications."
        ~docv:"INTERFACE")

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
          "Filename for the generated dune rules. For use on the command line."
        ~docv:"DUNE_OUTPUT")

let gen_alias =
  Arg.(
    value
    & opt (some string) None
    & info [ "gen-alias" ]
        ~doc:"Alias to which the OCaml code generation will be attached."
        ~docv:"GEN_ALIAS")

module Plugin : sig
  val cmd : unit Cmd.t
end = struct
  module Qcheck_stm : sig
    val cmd : unit Cmd.t
  end = struct
    let info =
      Cmd.info "qcheck-stm"
        ~doc:"Generate Dune rules for the qcheck-stm plugin."

    let config_file =
      Arg.(
        value
        & opt (some string) None
        & info [ "c"; "config" ]
            ~doc:
              "Configuration file for Ortac/QCheck-STM. Useful for generating \
               multiple test files per module under test."
            ~absent:
              "concatenation of INTERFACE without the extension and \
               \"_config.ml\""
            ~docv:"CONFIG")

    let library =
      Arg.(
        value
        & opt (some string) None
        & info [ "l"; "library" ]
            ~doc:"Name of the library the module under test belongs to."
            ~absent:"INTERFACE without the file extension" ~docv:"LIBRARY")

    let module_prefix =
      Arg.(
        value
        & opt (some string) None
        & info [ "m"; "module-prefix" ]
            ~doc:
              "Prefix for opening the module corresponding to FILE when it is \
               part of a library that has MODULE_PREFIX as name."
            ~docv:"MODULE_PREFIX")

    let submodule =
      Arg.(
        value
        & opt (some string) None
        & info [ "s"; "submodule" ]
            ~doc:"Build the qcheck-stm tests for SUBMODULE inside FILE"
            ~docv:"SUBMODULE")

    let domain =
      Arg.(
        value & flag & info [ "d"; "domain" ] ~doc:"Generate STM_domain tests.")

    let fork_timeout =
      Arg.(
        value
        & opt (some int) None
        & info [ "t"; "timeout" ] ~doc:"Timeout for each test." ~docv:"TIMEOUT")

    let main interface_file config_file ocaml_output library package_name
        dune_output module_prefix submodule domain fork_timeout gen_alias =
      let open Qcheck_stm in
      let config =
        {
          interface_file;
          config_file;
          ocaml_output;
          library;
          package_name;
          dune_output;
          module_prefix;
          submodule;
          domain;
          fork_timeout;
          gen_alias;
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
        $ with_stdout_to
        $ module_prefix
        $ submodule
        $ domain
        $ fork_timeout
        $ gen_alias)

    let cmd = Cmd.v info term
  end

  module Wrapper : sig
    val cmd : unit Cmd.t
  end = struct
    let info =
      Cmd.info "wrapper" ~doc:"Generate Dune rules for the wrapper plugin."

    let main interface_file package_name ocaml_output dune_output =
      let open Wrapper in
      let config =
        { interface_file; package_name; ocaml_output; dune_output }
      in
      let ppf = Registration.get_out_formatter dune_output in
      Wrapper.gen_dune_rules ppf config

    let term =
      Term.(
        const main
        $ interface_file
        $ package_name
        $ ocaml_output
        $ with_stdout_to)

    let cmd = Cmd.v info term
  end

  let cmd =
    let info = Cmd.info "dune" ~doc:"Generate Dune rule for ortac plugins." in
    Cmd.group info [ Qcheck_stm.cmd; Wrapper.cmd ]
end

let () = Registration.register Plugin.cmd
