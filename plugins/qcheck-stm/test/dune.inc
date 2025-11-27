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
     %{dep:array_config.ml}
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
  ortac-runtime-qcheck-stm.sequential
  array)
 (action
  (echo
   "\n%{dep:array_stm_tests.exe} has been generated with the ortac-qcheck-stm plugin.\n")))

(rule
 (alias runtest)
 (package ortac-qcheck-stm)
 (enabled_if
  (>= %{ocaml_version} 5.2))
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
     %{dep:hashtbl_config.ml}
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
  ortac-runtime-qcheck-stm.sequential
  hashtbl)
 (action
  (echo
   "\n%{dep:hashtbl_stm_tests.exe} has been generated with the ortac-qcheck-stm plugin.\n")))

(rule
 (alias runtest)
 (package ortac-qcheck-stm)
 (enabled_if
  (>= %{ocaml_version} 5.2))
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
     %{dep:record_config.ml}
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
  ortac-runtime-qcheck-stm.sequential
  record)
 (action
  (echo
   "\n%{dep:record_stm_tests.exe} has been generated with the ortac-qcheck-stm plugin.\n")))

(rule
 (alias runtest)
 (package ortac-qcheck-stm)
 (enabled_if
  (>= %{ocaml_version} 5.2))
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
     %{dep:ref_config.ml}
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
  ortac-runtime-qcheck-stm.sequential
  ref)
 (action
  (echo
   "\n%{dep:ref_stm_tests.exe} has been generated with the ortac-qcheck-stm plugin.\n")))

(rule
 (alias runtest)
 (package ortac-qcheck-stm)
 (enabled_if
  (>= %{ocaml_version} 5.2))
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
     %{dep:conjunctive_clauses_config.ml}
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
  ortac-runtime-qcheck-stm.sequential
  conjunctive_clauses)
 (action
  (echo
   "\n%{dep:conjunctive_clauses_stm_tests.exe} has been generated with the ortac-qcheck-stm plugin.\n")))

(rule
 (alias runtest)
 (package ortac-qcheck-stm)
 (enabled_if
  (>= %{ocaml_version} 5.2))
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
     %{dep:sequence_model_config.ml}
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
  ortac-runtime-qcheck-stm.sequential
  sequence_model)
 (action
  (echo
   "\n%{dep:sequence_model_stm_tests.exe} has been generated with the ortac-qcheck-stm plugin.\n")))

(rule
 (alias runtest)
 (package ortac-qcheck-stm)
 (enabled_if
  (>= %{ocaml_version} 5.2))
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
     %{dep:invariants_config.ml}
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
  ortac-runtime-qcheck-stm.sequential
  invariants)
 (action
  (echo
   "\n%{dep:invariants_stm_tests.exe} has been generated with the ortac-qcheck-stm plugin.\n")))

(rule
 (alias runtest)
 (package ortac-qcheck-stm)
 (enabled_if
  (>= %{ocaml_version} 5.2))
 (action
  (progn
   (diff invariants_errors.expected invariants_errors)
   (diff invariants_stm_tests.expected.ml invariants_stm_tests.ml))))

(rule
 (alias launchtests)
 (action
  (run %{dep:invariants_stm_tests.exe} -v)))

(library
 (name integer_in_model)
 (modules integer_in_model))

(rule
 (target integer_in_model_stm_tests.ml)
 (package ortac-qcheck-stm)
 (deps
  (package ortac-core)
  (package ortac-qcheck-stm))
 (action
  (setenv
   ORTAC_ONLY_PLUGIN
   qcheck-stm
   (with-stderr-to
    integer_in_model_errors
    (run
     ortac
     qcheck-stm
     %{dep:integer_in_model.mli}
     %{dep:integer_in_model_config.ml}
     -o
     %{target})))))

(test
 (name integer_in_model_stm_tests)
 (package ortac-qcheck-stm)
 (modules integer_in_model_stm_tests)
 (libraries
  qcheck-core
  qcheck-core.runner
  qcheck-stm.stm
  qcheck-stm.sequential
  qcheck-multicoretests-util
  ortac-runtime-qcheck-stm.sequential
  integer_in_model)
 (action
  (echo
   "\n%{dep:integer_in_model_stm_tests.exe} has been generated with the ortac-qcheck-stm plugin.\n")))

(rule
 (alias runtest)
 (package ortac-qcheck-stm)
 (enabled_if
  (>= %{ocaml_version} 5.2))
 (action
  (progn
   (diff integer_in_model_errors.expected integer_in_model_errors)
   (diff integer_in_model_stm_tests.expected.ml integer_in_model_stm_tests.ml))))

(rule
 (alias launchtests)
 (action
  (run %{dep:integer_in_model_stm_tests.exe} -v)))

(library
 (name ghost_as_model)
 (modules ghost_as_model))

(rule
 (target ghost_as_model_stm_tests.ml)
 (package ortac-qcheck-stm)
 (deps
  (package ortac-core)
  (package ortac-qcheck-stm))
 (action
  (setenv
   ORTAC_ONLY_PLUGIN
   qcheck-stm
   (with-stderr-to
    ghost_as_model_errors
    (run
     ortac
     qcheck-stm
     %{dep:ghost_as_model.mli}
     %{dep:ghost_as_model_config.ml}
     -o
     %{target})))))

(test
 (name ghost_as_model_stm_tests)
 (package ortac-qcheck-stm)
 (modules ghost_as_model_stm_tests)
 (libraries
  qcheck-core
  qcheck-core.runner
  qcheck-stm.stm
  qcheck-stm.sequential
  qcheck-multicoretests-util
  ortac-runtime-qcheck-stm.sequential
  ghost_as_model)
 (action
  (echo
   "\n%{dep:ghost_as_model_stm_tests.exe} has been generated with the ortac-qcheck-stm plugin.\n")))

(rule
 (alias runtest)
 (package ortac-qcheck-stm)
 (enabled_if
  (>= %{ocaml_version} 5.2))
 (action
  (progn
   (diff ghost_as_model_errors.expected ghost_as_model_errors)
   (diff ghost_as_model_stm_tests.expected.ml ghost_as_model_stm_tests.ml))))

(rule
 (alias launchtests)
 (action
  (run %{dep:ghost_as_model_stm_tests.exe} -v)))

(library
 (name custom_config)
 (modules custom_config))

(rule
 (target custom_config_stm_tests.ml)
 (package ortac-qcheck-stm)
 (deps
  (package ortac-core)
  (package ortac-qcheck-stm))
 (action
  (setenv
   ORTAC_ONLY_PLUGIN
   qcheck-stm
   (with-stderr-to
    custom_config_errors
    (run
     ortac
     qcheck-stm
     %{dep:custom_config.mli}
     %{dep:custom_config_config.ml}
     -o
     %{target})))))

(test
 (name custom_config_stm_tests)
 (package ortac-qcheck-stm)
 (modules custom_config_stm_tests)
 (libraries
  qcheck-core
  qcheck-core.runner
  qcheck-stm.stm
  qcheck-stm.sequential
  qcheck-multicoretests-util
  ortac-runtime-qcheck-stm.sequential
  custom_config)
 (action
  (echo
   "\n%{dep:custom_config_stm_tests.exe} has been generated with the ortac-qcheck-stm plugin.\n")))

(rule
 (alias runtest)
 (package ortac-qcheck-stm)
 (enabled_if
  (>= %{ocaml_version} 5.2))
 (action
  (progn
   (diff custom_config_errors.expected custom_config_errors)
   (diff custom_config_stm_tests.expected.ml custom_config_stm_tests.ml))))

(rule
 (alias launchtests)
 (action
  (run %{dep:custom_config_stm_tests.exe} -v)))

(library
 (name test_without_sut)
 (modules test_without_sut))

(rule
 (target test_without_sut_stm_tests.ml)
 (package ortac-qcheck-stm)
 (deps
  (package ortac-core)
  (package ortac-qcheck-stm))
 (action
  (setenv
   ORTAC_ONLY_PLUGIN
   qcheck-stm
   (with-stderr-to
    test_without_sut_errors
    (run
     ortac
     qcheck-stm
     %{dep:test_without_sut.mli}
     %{dep:test_without_sut_config.ml}
     -o
     %{target})))))

(test
 (name test_without_sut_stm_tests)
 (package ortac-qcheck-stm)
 (modules test_without_sut_stm_tests)
 (libraries
  qcheck-core
  qcheck-core.runner
  qcheck-stm.stm
  qcheck-stm.sequential
  qcheck-multicoretests-util
  ortac-runtime-qcheck-stm.sequential
  test_without_sut)
 (action
  (echo
   "\n%{dep:test_without_sut_stm_tests.exe} has been generated with the ortac-qcheck-stm plugin.\n")))

(rule
 (alias runtest)
 (package ortac-qcheck-stm)
 (enabled_if
  (>= %{ocaml_version} 5.2))
 (action
  (progn
   (diff test_without_sut_errors.expected test_without_sut_errors)
   (diff test_without_sut_stm_tests.expected.ml test_without_sut_stm_tests.ml))))

(rule
 (alias launchtests)
 (action
  (run %{dep:test_without_sut_stm_tests.exe} -v)))

(library
 (name tuples)
 (modules tuples))

(rule
 (target tuples_stm_tests.ml)
 (package ortac-qcheck-stm)
 (deps
  (package ortac-core)
  (package ortac-qcheck-stm))
 (action
  (setenv
   ORTAC_ONLY_PLUGIN
   qcheck-stm
   (with-stderr-to
    tuples_errors
    (run
     ortac
     qcheck-stm
     %{dep:tuples.mli}
     %{dep:tuples_config.ml}
     -o
     %{target})))))

(test
 (name tuples_stm_tests)
 (package ortac-qcheck-stm)
 (modules tuples_stm_tests)
 (libraries
  qcheck-core
  qcheck-core.runner
  qcheck-stm.stm
  qcheck-stm.sequential
  qcheck-multicoretests-util
  ortac-runtime-qcheck-stm.sequential
  tuples)
 (action
  (echo
   "\n%{dep:tuples_stm_tests.exe} has been generated with the ortac-qcheck-stm plugin.\n")))

(rule
 (alias runtest)
 (package ortac-qcheck-stm)
 (enabled_if
  (>= %{ocaml_version} 5.2))
 (action
  (progn
   (diff tuples_errors.expected tuples_errors)
   (diff tuples_stm_tests.expected.ml tuples_stm_tests.ml))))

(rule
 (alias launchtests)
 (action
  (run %{dep:tuples_stm_tests.exe} -v)))

(library
 (name functional_model)
 (modules functional_model))

(rule
 (target functional_model_stm_tests.ml)
 (package ortac-qcheck-stm)
 (deps
  (package ortac-core)
  (package ortac-qcheck-stm))
 (action
  (setenv
   ORTAC_ONLY_PLUGIN
   qcheck-stm
   (with-stderr-to
    functional_model_errors
    (run
     ortac
     qcheck-stm
     %{dep:functional_model.mli}
     %{dep:functional_model_config.ml}
     -o
     %{target})))))

(test
 (name functional_model_stm_tests)
 (package ortac-qcheck-stm)
 (modules functional_model_stm_tests)
 (libraries
  qcheck-core
  qcheck-core.runner
  qcheck-stm.stm
  qcheck-stm.sequential
  qcheck-multicoretests-util
  ortac-runtime-qcheck-stm.sequential
  functional_model)
 (action
  (echo
   "\n%{dep:functional_model_stm_tests.exe} has been generated with the ortac-qcheck-stm plugin.\n")))

(rule
 (alias runtest)
 (package ortac-qcheck-stm)
 (enabled_if
  (>= %{ocaml_version} 5.2))
 (action
  (progn
   (diff functional_model_errors.expected functional_model_errors)
   (diff functional_model_stm_tests.expected.ml functional_model_stm_tests.ml))))

(rule
 (alias launchtests)
 (action
  (run %{dep:functional_model_stm_tests.exe} -v)))

(library
 (name test_cleanup)
 (modules test_cleanup))

(rule
 (target test_cleanup_stm_tests.ml)
 (package ortac-qcheck-stm)
 (deps
  (package ortac-core)
  (package ortac-qcheck-stm))
 (action
  (setenv
   ORTAC_ONLY_PLUGIN
   qcheck-stm
   (with-stderr-to
    test_cleanup_errors
    (run
     ortac
     qcheck-stm
     %{dep:test_cleanup.mli}
     %{dep:test_cleanup_config.ml}
     -o
     %{target})))))

(test
 (name test_cleanup_stm_tests)
 (package ortac-qcheck-stm)
 (modules test_cleanup_stm_tests)
 (libraries
  qcheck-core
  qcheck-core.runner
  qcheck-stm.stm
  qcheck-stm.sequential
  qcheck-multicoretests-util
  ortac-runtime-qcheck-stm.sequential
  test_cleanup)
 (action
  (echo
   "\n%{dep:test_cleanup_stm_tests.exe} has been generated with the ortac-qcheck-stm plugin.\n")))

(rule
 (alias runtest)
 (package ortac-qcheck-stm)
 (enabled_if
  (>= %{ocaml_version} 5.2))
 (action
  (progn
   (diff test_cleanup_errors.expected test_cleanup_errors)
   (diff test_cleanup_stm_tests.expected.ml test_cleanup_stm_tests.ml))))

(rule
 (alias launchtests)
 (action
  (run %{dep:test_cleanup_stm_tests.exe} -v)))

(library
 (name sut_in_type)
 (modules sut_in_type))

(rule
 (target sut_in_type_stm_tests.ml)
 (package ortac-qcheck-stm)
 (deps
  (package ortac-core)
  (package ortac-qcheck-stm))
 (action
  (setenv
   ORTAC_ONLY_PLUGIN
   qcheck-stm
   (with-stderr-to
    sut_in_type_errors
    (run
     ortac
     qcheck-stm
     %{dep:sut_in_type.mli}
     %{dep:sut_in_type_config.ml}
     -o
     %{target})))))

(test
 (name sut_in_type_stm_tests)
 (package ortac-qcheck-stm)
 (modules sut_in_type_stm_tests)
 (libraries
  qcheck-core
  qcheck-core.runner
  qcheck-stm.stm
  qcheck-stm.sequential
  qcheck-multicoretests-util
  ortac-runtime-qcheck-stm.sequential
  sut_in_type)
 (action
  (echo
   "\n%{dep:sut_in_type_stm_tests.exe} has been generated with the ortac-qcheck-stm plugin.\n")))

(rule
 (alias runtest)
 (package ortac-qcheck-stm)
 (enabled_if
  (>= %{ocaml_version} 5.2))
 (action
  (progn
   (diff sut_in_type_errors.expected sut_in_type_errors)
   (diff sut_in_type_stm_tests.expected.ml sut_in_type_stm_tests.ml))))

(rule
 (alias launchtests)
 (action
  (run %{dep:sut_in_type_stm_tests.exe} -v)))

(library
 (name queue)
 (modules queue))

(rule
 (target queue_stm_tests.ml)
 (package ortac-qcheck-stm)
 (deps
  (package ortac-core)
  (package ortac-qcheck-stm))
 (action
  (setenv
   ORTAC_ONLY_PLUGIN
   qcheck-stm
   (with-stderr-to
    queue_errors
    (run
     ortac
     qcheck-stm
     %{dep:queue.mli}
     %{dep:queue_config.ml}
     -o
     %{target})))))

(test
 (name queue_stm_tests)
 (package ortac-qcheck-stm)
 (modules queue_stm_tests)
 (libraries
  qcheck-core
  qcheck-core.runner
  qcheck-stm.stm
  qcheck-stm.sequential
  qcheck-multicoretests-util
  ortac-runtime-qcheck-stm.sequential
  queue)
 (action
  (echo
   "\n%{dep:queue_stm_tests.exe} has been generated with the ortac-qcheck-stm plugin.\n")))

(rule
 (alias runtest)
 (package ortac-qcheck-stm)
 (enabled_if
  (>= %{ocaml_version} 5.2))
 (action
  (progn
   (diff queue_errors.expected queue_errors)
   (diff queue_stm_tests.expected.ml queue_stm_tests.ml))))

(rule
 (alias launchtests)
 (action
  (run %{dep:queue_stm_tests.exe} -v)))

(library
 (name stack)
 (modules stack))

(rule
 (target stack_stm_tests.ml)
 (package ortac-qcheck-stm)
 (deps
  (package ortac-core)
  (package ortac-qcheck-stm))
 (action
  (setenv
   ORTAC_ONLY_PLUGIN
   qcheck-stm
   (with-stderr-to
    stack_errors
    (run
     ortac
     qcheck-stm
     %{dep:stack.mli}
     %{dep:stack_config.ml}
     -o
     %{target})))))

(test
 (name stack_stm_tests)
 (package ortac-qcheck-stm)
 (modules stack_stm_tests)
 (libraries
  qcheck-core
  qcheck-core.runner
  qcheck-stm.stm
  qcheck-stm.sequential
  qcheck-multicoretests-util
  ortac-runtime-qcheck-stm.sequential
  stack)
 (action
  (echo
   "\n%{dep:stack_stm_tests.exe} has been generated with the ortac-qcheck-stm plugin.\n")))

(rule
 (alias runtest)
 (package ortac-qcheck-stm)
 (enabled_if
  (>= %{ocaml_version} 5.2))
 (action
  (progn
   (diff stack_errors.expected stack_errors)
   (diff stack_stm_tests.expected.ml stack_stm_tests.ml))))

(rule
 (alias launchtests)
 (action
  (run %{dep:stack_stm_tests.exe} -v)))

(library
 (name function_args)
 (modules function_args))

(rule
 (target function_args_stm_tests.ml)
 (package ortac-qcheck-stm)
 (deps
  (package ortac-core)
  (package ortac-qcheck-stm))
 (action
  (setenv
   ORTAC_ONLY_PLUGIN
   qcheck-stm
   (with-stderr-to
    function_args_errors
    (run
     ortac
     qcheck-stm
     %{dep:function_args.mli}
     %{dep:function_args_config.ml}
     -o
     %{target})))))

(test
 (name function_args_stm_tests)
 (package ortac-qcheck-stm)
 (modules function_args_stm_tests)
 (libraries
  qcheck-core
  qcheck-core.runner
  qcheck-stm.stm
  qcheck-stm.sequential
  qcheck-multicoretests-util
  ortac-runtime-qcheck-stm.sequential
  function_args)
 (action
  (echo
   "\n%{dep:function_args_stm_tests.exe} has been generated with the ortac-qcheck-stm plugin.\n")))

(rule
 (alias runtest)
 (package ortac-qcheck-stm)
 (enabled_if
  (>= %{ocaml_version} 5.2))
 (action
  (progn
   (diff function_args_errors.expected function_args_errors)
   (diff function_args_stm_tests.expected.ml function_args_stm_tests.ml))))

(rule
 (alias launchtests)
 (action
  (run %{dep:function_args_stm_tests.exe} -v)))

