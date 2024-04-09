let () =
  match Sys.getenv_opt "ORTAC_ONLY_PLUGIN" with
  | None -> Sites.Plugins.Plugins.load_all ()
  | Some plug -> Sites.Plugins.Plugins.load plug

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
      let version =
        Printf.sprintf "ortac version: %s"
          (match Build_info.V1.version () with
          | None -> "n/a"
          | Some v -> Build_info.V1.Version.to_string v)
      in
      let info = Cmd.info "ortac" ~doc ~version in
      let group = Cmd.group info cmds in
      Stdlib.exit (Cmd.eval group)
