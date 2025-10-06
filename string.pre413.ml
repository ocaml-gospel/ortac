include Stdlib.String

(* Before 4.13, starts_with is not provided *)
let starts_with ~prefix word =
  let rec aux pos =
    pos < 0 || (get prefix pos = get word pos && aux (pos - 1))
  in
  length prefix <= length word && aux (length prefix - 1)
