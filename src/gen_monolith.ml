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
