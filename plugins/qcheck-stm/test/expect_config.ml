open Ortac_qcheck_stm

let underline str =
  Fmt.pr "@[<v 0>%s@,%s@]@." str (String.map (Fun.const '=') str)

let pp = Reserr.pp (Fmt.pair Fmt.nop Config.dump) Fmt.stdout
let () = underline "test with correct cli arguments:"
let () = Config.init "lib.mli" "make 'a' 1" "(char, int) t" |> pp
let () = underline "test with non-existing sut type:"
let () = Config.init "lib.mli" "make 'a' 1" "absent" |> pp
let () = underline "test with ill-formed init_sut expression:"
let () = Config.init "lib.mli" "make (" "(char, int) t" |> pp
let () = underline "test with ill-formed type expression:"
let () = Config.init "lib.mli" "make () ()" "a-t" |> pp
let () = underline "test with non instantiated sut type parameter:"
let () = Config.init "lib.mli" "make () ()" "('a, int) t" |> pp
let () = underline "test with arrow type as sut:"
let () = Config.init "lib.mli" "make () ()" "int -> int" |> pp
