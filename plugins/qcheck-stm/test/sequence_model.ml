type 'a t = 'a list ref

let create () = ref []
let add x s = s := x :: !s

let remove s =
  match !s with
  | [] -> None
  | x :: xs ->
      s := xs;
      Some x

let remove_ = remove
