let () = Sites.Plugins.Plugins.load_all ()

open Cmdliner

let group =
  let cmds =
    Registration.fold (fun acc cmd -> cmd :: acc) [] Registration.plugins
  in
  let doc = "Run ORTAC." in
  let version = "ortac version %%VERSION%%" in
  let info = Cmd.info "ortac" ~doc ~version in
  Cmd.group info cmds

let () = Stdlib.exit (Cmd.eval group)
