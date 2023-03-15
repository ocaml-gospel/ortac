let main () = print_endline "Hello"

open Cmdliner

module Plugin : sig
  val cmd : unit Cmd.t
end = struct
  let info =
    Cmd.info "qcheck-stm"
      ~doc:"Generate QCheck-stm test file according to Gospel specifications."

  let term = Term.(const main $ const ())
  let cmd = Cmd.v info term
end

let () = Registration.register Plugin.cmd
