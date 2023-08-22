let () = Sites.Plugins.Plugins.load_all ()

open Cmdliner

let usage () =
  Format.(fprintf err_formatter)
    "@[ortac: required plugin is missing, please install at least one@ \
     (qcheck-stm,@ monolith@ or@ wrapper).@]@.";
  exit Cmd.Exit.cli_error

let () =
  match
    Registration.fold (fun acc cmd -> cmd :: acc) [] Registration.plugins
  with
  | [] -> usage ()
  | cmds ->
      let doc = "Run ORTAC." in
      let version = "ortac version %%VERSION%%" in
      let info = Cmd.info "ortac" ~doc ~version in
      let group = Cmd.group info cmds in
      Stdlib.exit (Cmd.eval group)
