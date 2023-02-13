let () = Sites.Plugins.Plugins.load_all ()

open Registration
open Cmdliner

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

let group =
  let cmds =
    Registration.fold
      (fun acc cmd -> cmd :: acc)
      [ Default.cmd ] Registration.plugins
  in
  let doc = "Run ORTAC." in
  let version = "ortac version %%VERSION%%" in
  let info = Cmd.info "ortac" ~doc ~version in
  Cmd.group info cmds

let () = Stdlib.exit (Cmd.eval group)
