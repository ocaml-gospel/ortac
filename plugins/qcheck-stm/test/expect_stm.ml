let () = print_endline "test with existing sut type:"
let () = Ortac_qcheck_stm.main "lib.mli" "" "sut"
let () = print_newline ()
let () = print_endline "test with non-existing sut type:"
let () = Ortac_qcheck_stm.main "lib.mli" "" "absent"
