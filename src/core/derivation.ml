open Gospel

module Common = struct
  module W = Warnings
  open Ppxlib

  type operator = Eq | Leq | Geq | Lt | Gt

  let op2expr ~loc = function
    | Eq -> [%expr ( = )]
    | Leq -> [%expr ( <= )]
    | Geq -> [%expr ( >= )]
    | Lt -> [%expr ( < )]
    | Gt -> [%expr ( > )]

  let integer ~loc = function
    | Eq -> [%expr ( = )]
    | Leq -> [%expr Ortac_runtime.Z.leq]
    | Geq -> [%expr Ortac_runtime.Z.geq]
    | Lt -> [%expr Ortac_runtime.Z.lt]
    | Gt -> [%expr Ortac_runtime.Z.gt]

  let ls2op (ls : Tterm.lsymbol) =
    match ls.ls_name.id_str with
    | "infix =" -> Eq
    | "infix <=" -> Leq
    | "infix >=" -> Geq
    | "infix <" -> Lt
    | "infix >" -> Gt
    | _ -> assert false

  let core drv (ts : Ttypes.tysymbol) =
    let ts_int = Drv.get_ts drv [ "Gospelstdlib"; "int" ] in
    let core =
      let open Ttypes in
      [ ts_unit; ts_bool; ts_char; ts_int; ts_integer; ts_string ]
    in
    List.exists (Ttypes.ts_equal ts) core

  let rec derive ~drv operator (ty : Ttypes.ty) =
    let loc = Location.none in
    match ty.ty_node with
    | Tyvar _tv -> Ok (op2expr ~loc operator)
    | Tyapp (ts, _) when Ttypes.ts_equal ts Ttypes.ts_integer ->
        Ok (integer ~loc operator)
    | Tyapp (ts, _) when core drv ts -> Ok (op2expr ~loc operator)
    | Tyapp (ts, [ ty ]) when Ttypes.ts_equal ts Ttypes.ts_list ->
        derive ~drv operator ty |> Result.map (fun _ -> op2expr ~loc operator)
    | Tyapp (ts, [ ty ])
      when Ttypes.ts_equal ts (Drv.get_ts drv [ "Gospelstdlib"; "array" ]) ->
        derive ~drv operator ty |> Result.map (fun _ -> op2expr ~loc operator)
    | Tyapp (ts, _) -> Error (W.Unsupported_comparison ts.ts_ident.id_str)
end

let is_derivable (ls : Tterm.lsymbol) =
  List.mem ls.ls_name.id_str
    [ "infix >="; "infix >"; "infix <="; "infix <"; "infix =" ]

let derive drv (ls : Tterm.lsymbol) =
  let ts = List.hd ls.ls_args in
  let op = Common.ls2op ls in
  Common.derive ~drv op ts
