module Config = Config
module Ir = Ir
module Ir_of_gospel = Ir_of_gospel
module Reserr = Reserr
module Stm_of_ir = Stm_of_ir

let main path init sut =
  let open Reserr in
  let pp = Fmt.((pp Ppxlib_ast.Pprintast.structure) stdout) in
  let _ =
    let* sigs, config = Config.init path init sut in
    let* ir = Ir_of_gospel.run sigs config in
    Stm_of_ir.stm config ir |> pp |> ok
  in
  ()

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
      & pos 2 (some string) None
      & info [] ~doc:"Build the qcheck-stm tests with SUT." ~docv:"SUT")

  let init =
    Arg.(
      required
      & pos 1 (some string) None
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
