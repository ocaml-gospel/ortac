type error = |
type 'a reserr = ('a, error list) result

let ok = Result.ok
let error e = Result.error [ e ]
let ( let* ) = Result.bind

let ( and* ) a b =
  match (a, b) with
  | Error e0, Error e1 -> Error (e0 @ e1)
  | Error e, _ | _, Error e -> Error e
  | Ok a, Ok b -> Ok (a, b)
