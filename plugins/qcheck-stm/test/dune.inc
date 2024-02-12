(library
 (name array)
 (modules array))

(rule
 (target array_stm_tests.ml)
 (package ortac-qcheck-stm)
 (deps
  (package ortac-core)
  (package ortac-qcheck-stm))
 (action
  (setenv
   ORTAC_ONLY_PLUGIN
   qcheck-stm
   (with-stderr-to
    array_errors
    (run
     ortac
     qcheck-stm
     %{dep:array.mli}
     "make 16 'a'" "char t"
     -o
     %{target})))))

(test
 (name array_stm_tests)
 (package ortac-qcheck-stm)
 (modules array_stm_tests)
 (libraries
  qcheck-core
  qcheck-core.runner
  qcheck-stm.stm
  qcheck-stm.sequential
  qcheck-multicoretests-util
  ortac-runtime-qcheck-stm
  array)
 (action
  (echo
   "\n%{dep:array_stm_tests.exe} has been generated with the ortac-qcheck-stm plugin.\n")))

(rule
 (alias runtest)
 (package ortac-qcheck-stm)
 (action
  (progn
   (diff array_errors.expected array_errors)
   (diff array_stm_tests.expected.ml array_stm_tests.ml))))

(rule
 (alias launchtests)
 (action
  (run %{dep:array_stm_tests.exe} -v)))

(library
 (name hashtbl)
 (modules hashtbl))

(rule
 (target hashtbl_stm_tests.ml)
 (package ortac-qcheck-stm)
 (deps
  (package ortac-core)
  (package ortac-qcheck-stm))
 (action
  (setenv
   ORTAC_ONLY_PLUGIN
   qcheck-stm
   (with-stderr-to
    hashtbl_errors
    (run
     ortac
     qcheck-stm
     %{dep:hashtbl.mli}
     "create ~random:false 16" "(char, int) t"
     -o
     %{target})))))

(test
 (name hashtbl_stm_tests)
 (package ortac-qcheck-stm)
 (modules hashtbl_stm_tests)
 (libraries
  qcheck-core
  qcheck-core.runner
  qcheck-stm.stm
  qcheck-stm.sequential
  qcheck-multicoretests-util
  ortac-runtime-qcheck-stm
  hashtbl)
 (action
  (echo
   "\n%{dep:hashtbl_stm_tests.exe} has been generated with the ortac-qcheck-stm plugin.\n")))

(rule
 (alias runtest)
 (package ortac-qcheck-stm)
 (action
  (progn
   (diff hashtbl_errors.expected hashtbl_errors)
   (diff hashtbl_stm_tests.expected.ml hashtbl_stm_tests.ml))))

(rule
 (alias launchtests)
 (action
  (run %{dep:hashtbl_stm_tests.exe} -v)))

(library
 (name record)
 (modules record))

(rule
 (target record_stm_tests.ml)
 (package ortac-qcheck-stm)
 (deps
  (package ortac-core)
  (package ortac-qcheck-stm))
 (action
  (setenv
   ORTAC_ONLY_PLUGIN
   qcheck-stm
   (with-stderr-to
    record_errors
    (run
     ortac
     qcheck-stm
     %{dep:record.mli}
     "make 42" "t"
     -o
     %{target})))))

(test
 (name record_stm_tests)
 (package ortac-qcheck-stm)
 (modules record_stm_tests)
 (libraries
  qcheck-core
  qcheck-core.runner
  qcheck-stm.stm
  qcheck-stm.sequential
  qcheck-multicoretests-util
  ortac-runtime-qcheck-stm
  record)
 (action
  (echo
   "\n%{dep:record_stm_tests.exe} has been generated with the ortac-qcheck-stm plugin.\n")))

(rule
 (alias runtest)
 (package ortac-qcheck-stm)
 (action
  (progn
   (diff record_errors.expected record_errors)
   (diff record_stm_tests.expected.ml record_stm_tests.ml))))

(rule
 (alias launchtests)
 (action
  (run %{dep:record_stm_tests.exe} -v)))

(library
 (name ref)
 (modules ref))

(rule
 (target ref_stm_tests.ml)
 (package ortac-qcheck-stm)
 (deps
  (package ortac-core)
  (package ortac-qcheck-stm))
 (action
  (setenv
   ORTAC_ONLY_PLUGIN
   qcheck-stm
   (with-stderr-to
    ref_errors
    (run
     ortac
     qcheck-stm
     %{dep:ref.mli}
     "make 42" "t"
     -o
     %{target})))))

(test
 (name ref_stm_tests)
 (package ortac-qcheck-stm)
 (modules ref_stm_tests)
 (libraries
  qcheck-core
  qcheck-core.runner
  qcheck-stm.stm
  qcheck-stm.sequential
  qcheck-multicoretests-util
  ortac-runtime-qcheck-stm
  ref)
 (action
  (echo
   "\n%{dep:ref_stm_tests.exe} has been generated with the ortac-qcheck-stm plugin.\n")))

(rule
 (alias runtest)
 (package ortac-qcheck-stm)
 (action
  (progn
   (diff ref_errors.expected ref_errors)
   (diff ref_stm_tests.expected.ml ref_stm_tests.ml))))

(rule
 (alias launchtests)
 (action
  (run %{dep:ref_stm_tests.exe} -v)))

(library
 (name conjunctive_clauses)
 (modules conjunctive_clauses))

(rule
 (target conjunctive_clauses_stm_tests.ml)
 (package ortac-qcheck-stm)
 (deps
  (package ortac-core)
  (package ortac-qcheck-stm))
 (action
  (setenv
   ORTAC_ONLY_PLUGIN
   qcheck-stm
   (with-stderr-to
    conjunctive_clauses_errors
    (run
     ortac
     qcheck-stm
     %{dep:conjunctive_clauses.mli}
     "make 42 'a'" "char t"
     -o
     %{target})))))

(test
 (name conjunctive_clauses_stm_tests)
 (package ortac-qcheck-stm)
 (modules conjunctive_clauses_stm_tests)
 (libraries
  qcheck-core
  qcheck-core.runner
  qcheck-stm.stm
  qcheck-stm.sequential
  qcheck-multicoretests-util
  ortac-runtime-qcheck-stm
  conjunctive_clauses)
 (action
  (echo
   "\n%{dep:conjunctive_clauses_stm_tests.exe} has been generated with the ortac-qcheck-stm plugin.\n")))

(rule
 (alias runtest)
 (package ortac-qcheck-stm)
 (action
  (progn
   (diff conjunctive_clauses_errors.expected conjunctive_clauses_errors)
   (diff conjunctive_clauses_stm_tests.expected.ml conjunctive_clauses_stm_tests.ml))))

(rule
 (alias launchtests)
 (action
  (run %{dep:conjunctive_clauses_stm_tests.exe} -v)))

(library
 (name sequence_model)
 (modules sequence_model))

(rule
 (target sequence_model_stm_tests.ml)
 (package ortac-qcheck-stm)
 (deps
  (package ortac-core)
  (package ortac-qcheck-stm))
 (action
  (setenv
   ORTAC_ONLY_PLUGIN
   qcheck-stm
   (with-stderr-to
    sequence_model_errors
    (run
     ortac
     qcheck-stm
     %{dep:sequence_model.mli}
     "create ()" "char t"
     -o
     %{target})))))

(test
 (name sequence_model_stm_tests)
 (package ortac-qcheck-stm)
 (modules sequence_model_stm_tests)
 (libraries
  qcheck-core
  qcheck-core.runner
  qcheck-stm.stm
  qcheck-stm.sequential
  qcheck-multicoretests-util
  ortac-runtime-qcheck-stm
  sequence_model)
 (action
  (echo
   "\n%{dep:sequence_model_stm_tests.exe} has been generated with the ortac-qcheck-stm plugin.\n")))

(rule
 (alias runtest)
 (package ortac-qcheck-stm)
 (action
  (progn
   (diff sequence_model_errors.expected sequence_model_errors)
   (diff sequence_model_stm_tests.expected.ml sequence_model_stm_tests.ml))))

(rule
 (alias launchtests)
 (action
  (run %{dep:sequence_model_stm_tests.exe} -v)))

(library
 (name invariants)
 (modules invariants))

(rule
 (target invariants_stm_tests.ml)
 (package ortac-qcheck-stm)
 (deps
  (package ortac-core)
  (package ortac-qcheck-stm))
 (action
  (setenv
   ORTAC_ONLY_PLUGIN
   qcheck-stm
   (with-stderr-to
    invariants_errors
    (run
     ortac
     qcheck-stm
     %{dep:invariants.mli}
     "create 42" "int t"
     -o
     %{target})))))

(test
 (name invariants_stm_tests)
 (package ortac-qcheck-stm)
 (modules invariants_stm_tests)
 (libraries
  qcheck-core
  qcheck-core.runner
  qcheck-stm.stm
  qcheck-stm.sequential
  qcheck-multicoretests-util
  ortac-runtime-qcheck-stm
  invariants)
 (action
  (echo
   "\n%{dep:invariants_stm_tests.exe} has been generated with the ortac-qcheck-stm plugin.\n")))

(rule
 (alias runtest)
 (package ortac-qcheck-stm)
 (action
  (progn
   (diff invariants_errors.expected invariants_errors)
   (diff invariants_stm_tests.expected.ml invariants_stm_tests.ml))))

(rule
 (alias launchtests)
 (action
  (run %{dep:invariants_stm_tests.exe} -v)))

