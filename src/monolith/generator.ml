module type G = sig
  val open_modules : Ppxlib.structure_item list

  val report_pre : string -> Ppxlib.expression

  val report_post : string -> Ppxlib.expression

  val report_declared_exn : string -> string -> Ppxlib.expression

  val report_undeclared_exn :
    Ppxlib.expression -> string -> string -> Ppxlib.expression
end

module M : G = struct
  open Ppxlib

  include Ast_builder.Make (struct
    let loc = Location.none
  end)

  let open_modules = [ [%stri open Gospel_runtime]; [%stri open Monolith] ]

  let report_pre acc_name =
    [%expr
      Errors.check_and_do
        (fun _ -> raise Monolith.PleaseBackOff)
        [%e evar acc_name]]

  let report_post acc_name =
    [%expr Errors.check_and_do Errors.report_and_raise [%e evar acc_name]]

  let report_declared_exn acc_name alias =
    [%expr
      Errors.check_and_do Errors.report_and_raise [%e evar acc_name];
      raise [%e evar alias]]

  let report_undeclared_exn eloc fun_name acc_name =
    [%expr
      let err = mk_unexpected_exception [%e eloc] [%e estring fun_name] e in
      Errors.register err [%e evar acc_name];
      Errors.report [%e evar acc_name];
      Errors.raise_errors [%e evar acc_name];
      (* Here for typechecking reason *)
      failwith "This portion of code shouldn't be accessible"]
end

module B = Gospel_rtac_core.Builder.Make (M)
module G = Gospel_rtac_core.Gospel_rtac.Make (M)

let generate module_name s =
  try
    let include_lib =
      B.pmod_ident (B.lident module_name) |> B.include_infos |> B.pstr_include
    in
    let declarations = G.signature s in
    B.mk_open @ include_lib :: declarations
  with
  | B.Unsupported (_loc, msg) ->
      let open Fmt in
      failwith "%a: unsupported %s" (styled `Red string) "Error" msg
  | e -> raise e
