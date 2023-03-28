module Ident = Gospel.Identifier.Ident

type error =
  | Value_is_a_constant of (Ppxlib.location * Ident.t)
  | Value_return_sut of (Ppxlib.location * Ident.t)
  | Value_have_no_sut_argument of (Ppxlib.location * Ident.t)
  | Value_have_multiple_sut_arguments of (Ppxlib.location * Ident.t)

let error_to_string = function
  | Value_is_a_constant (_loc, id) -> "Value `" ^ id.id_str ^ "' is a constant."
  | Value_return_sut (_loc, id) -> "Value `" ^ id.id_str ^ "' returns a sut."
  | Value_have_no_sut_argument (_loc, id) ->
      "Value `" ^ id.id_str ^ "' have no sut argument"
  | Value_have_multiple_sut_arguments (_loc, id) ->
      "Value `" ^ id.id_str ^ "' have multiple sut arguments."

type 'a reserr = ('a, error list) result

let ok = Result.ok
let error e = Result.error [ e ]
let ( let* ) = Result.bind

let ( and* ) a b =
  match (a, b) with
  | Error e0, Error e1 -> Error (e0 @ e1)
  | Error e, _ | _, Error e -> Error e
  | Ok a, Ok b -> Ok (a, b)

let to_string pp = function
  | Ok a -> pp a
  | Error e -> String.concat "\n" (List.map error_to_string e)
