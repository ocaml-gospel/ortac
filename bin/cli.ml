let () =
  match Sys.getenv_opt "ORTAC_ONLY_PLUGIN" with
  | None -> Sites.Plugins.Plugins.load_all ()
  | Some plug -> Sites.Plugins.Plugins.load plug

open Cmdliner

let default_cmd cmds =
  let errmsg, usage =
    match cmds with
    | [] ->
        (* We cannot be in the case where ORTAC_ONLY_PLUGIN was set, since
           trying to load only one missing plugin would have raised an exception
           earlier, so we know no plugin is available *)
        (* Displaying usage in that case is useless *)
        ("error: no plugin is available, please install at least one", false)
    | _ -> ("error: missing command", true)
  in
  Term.(term_result ~usage (const (Result.error (`Msg errmsg))))

let () =
  let doc = "Run ORTAC."
  and version =
    Printf.sprintf "ortac version: %s"
      (match Build_info.V1.version () with
      | None -> "n/a"
      | Some v -> Build_info.V1.Version.to_string v)
  in
  let info = Cmd.info "ortac" ~doc ~version
  and cmds =
    Registration.fold (fun acc cmd -> cmd :: acc) [] Registration.plugins
  in
  let default = default_cmd cmds in
  let group = Cmd.group info ~default cmds in
  Stdlib.exit (Cmd.eval group)
