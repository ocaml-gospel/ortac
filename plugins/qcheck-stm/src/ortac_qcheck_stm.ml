module Config = Config
module Ir = Ir
module Ir_of_gospel = Ir_of_gospel
module Reserr = Reserr
module Stm_of_ir = Stm_of_ir

let remove_ppxlib_attributes =
  let open Ppxlib in
  object
    inherit Ast_traverse.map as super

    method! expression (expr : expression) =
      match expr.pexp_desc with
      | Pexp_fun _ ->
          let pexp_attributes =
            List.filter
              (fun a ->
                not
                @@ String.starts_with ~prefix:"ppxlib.migration" a.attr_name.txt)
              expr.pexp_attributes
          in
          super#expression { expr with pexp_attributes }
      | Pexp_function _ ->
          let pexp_attributes =
            List.filter
              (fun a ->
                not
                @@ String.starts_with ~prefix:"ppxlib.migration" a.attr_name.txt)
              expr.pexp_attributes
          in
          super#expression { expr with pexp_attributes }
      | _ -> super#expression expr
  end

let remove_attrs = remove_ppxlib_attributes#structure

let main path config output module_prefix submodule domain quiet count () =
  let open Reserr in
  let fmt = Registration.get_out_formatter output in
  let pp = pp quiet Ppxlib_ast.Pprintast.structure fmt in
  pp
    (let* sigs, config =
       Config.init path config module_prefix submodule domain count
     in
     let* ir = Ir_of_gospel.run sigs config in
     remove_attrs <$> Stm_of_ir.stm config ir)

open Cmdliner

module Plugin : sig
  val cmd : unit Cmd.t
end = struct
  let info =
    Cmd.info "qcheck-stm"
      ~doc:"Generate QCheck-stm test file according to Gospel specifications."

  let module_prefix =
    let parse s = Ok (Some s) and docv = "MODULE_PREFIX" in
    Arg.(
      value
      & opt (conv ~docv (parse, Fmt.(option string))) None
      & info [ "m"; "module-prefix" ] ~absent:"" ~docv
          ~doc:
            "Prefix for opening the module corresponding to FILE when it is \
             part of a library that has MODULE_PREFIX as name.")

  let submodule =
    let parse s = Ok (Some s) and docv = "SUBMODULE" in
    Arg.(
      value
      & opt (conv ~docv (parse, Fmt.(option string))) None
      & info [ "s"; "submodule" ] ~absent:"" ~docv
          ~doc:"Build the qcheck-stm tests for SUBMODULE inside FILE")

  let config =
    Arg.(
      required
      & pos 1 (some string) None
      & info []
          ~doc:
            "Build the qcheck-stm tests using CONFIG module as configuration \
             file."
          ~docv:"CONFIG")

  let domain =
    Arg.(
      value & flag & info [ "d"; "domain" ] ~doc:"Generate STM_domain tests.")

  let count =
    let parse i = Ok (int_of_string i) and docv = "COUNT" in
    Arg.(
      value
      & opt (conv ~docv (parse, Fmt.(int))) 1000
      & info [ "count" ] ~absent:"1000"
          ~doc:"Build STM tests with COUNT test iterations.")

  let term =
    let open Registration in
    Term.(
      const main
      $ ocaml_file
      $ config
      $ output_file
      $ module_prefix
      $ submodule
      $ domain
      $ quiet
      $ count
      $ setup_log)

  let cmd = Cmd.v info term
end

let () = Registration.register Plugin.cmd
