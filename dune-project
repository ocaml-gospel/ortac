(lang dune 3.8)

(generate_opam_files)

(using dune_site 0.1)

(using mdx 0.4)

(version 0.7.1)

(name ortac)

(license MIT)

(source
 (github ocaml-gospel/ortac))

(maintainers "Nicolas Osborne <nicolas.osborne@tarides.com>")

(authors
 "Clément Pascutto <clement@pascutto.fr>"
 "Nicolas Osborne <nicolas.osborne@tarides.com>"
 "Samuel Hym <samuel@tarides.com>")

(package
 (name ortac-core)
 (synopsis
  "Ortac (OCaml Runtime Assertion Checking) core tool and library based on Gospel")
 (description "")
 (documentation https://ocaml-gospel.github.io/ortac/ortac-core/)
 (depends
  (ocaml
   (>= 4.12.0))
  dune-build-info
  dune-site
  (cmdliner
   (>= 1.1.0))
  fmt
  (ppxlib
   (>= 0.26.0))
  (gospel
   (= 0.3.0))
  (alcotest
   (and
    :with-test
    (>= 0.8.1)))
  (ortac-runtime
   (and
    :with-test
    (= :version))))
 (sites
  (lib plugins))
 (conflicts
  (result
   (< 1.5))))

(package
 (name ortac-runtime)
 (synopsis "Runtime support library for Ortac-generated code")
 (description "")
 (documentation https://ocaml-gospel.github.io/ortac/ortac-runtime/)
 (depends
  (ocaml
   (>= 4.12.0))
  (fmt
   (>= 0.8.7))
  zarith
  (monolith
   (and
    :with-test
    (>= 20201026)))))

(package
 (name ortac-wrapper)
 (synopsis "Wrapper plugin for Ortac")
 (description "")
 (authors
  "Clément Pascutto <clement@pascutto.fr>"
  "Nicolas Osborne <nicolas.osborne@tarides.com>"
  "Charlène Gros <charlène@tarides.com>")
 (depends
  (ocaml
   (>= 4.12.0))
  (cmdliner
   (>= 1.1.0))
  fmt
  (ppxlib
   (>= 0.26.0))
  (gospel
   (= 0.3.0))
  (alcotest
   (and
    :with-test
    (>= 0.8.1)))
  (ortac-core
   (= :version))
  (ortac-runtime
   (= :version))))

(package
 (name ortac-monolith)
 (synopsis "Monolith plugin for Ortac")
 (description "")
 (authors "Nicolas Osborne <nicolas.osborne@tarides.com>")
 (depends
  (ocaml
   (>= 4.12.0))
  (cmdliner
   (>= 1.1.0))
  fmt
  (ppxlib
   (>= 0.26.0))
  (gospel
   (= 0.3.0))
  (monolith
   (and
    :with-test
    (>= 20201026)))
  (pprint :with-test)
  (ortac-core
   (= :version))
  ortac-wrapper
  (ortac-runtime-monolith :with-test)))

(package
 (name ortac-runtime-monolith)
 (synopsis "Runtime support library for Ortac/Monolith-generated code")
 (description "")
 (authors "Nicolas Osborne <nicolas.osborne@tarides.com>")
 (depends
  (ocaml
   (>= 4.12.0))
  (monolith
   (>= 20201026))
  pprint
  (fmt
   (>= 0.9.0))
  (ortac-runtime
   (= :version))))

(package
 (name ortac-qcheck-stm)
 (synopsis "QCheck-STM plugin for Ortac")
 (description "")
 (authors
  "Nicolas Osborne <nicolas.osborne@tarides.com>"
  "Samuel Hym <samuel@tarides.com>"
  "Nikolaus Huber <mail@n-huber.eu>")
 (documentation https://ocaml-gospel.github.io/ortac/ortac-qcheck-stm/)
 (depends
  (qcheck-multicoretests-util
   (and
    (>= 0.8)
    :with-test))
  (qcheck-core
   (and
    (>= 0.25)
    :with-test))
  (qcheck-multicoretests-util
   (and
    (>= 0.8)
    :with-test))
  (qcheck-core
   (and
    (>= 0.25)
    :with-test))
  (ocaml
   (>= 4.12.0))
  (cmdliner
   (>= 1.1.0))
  fmt
  (ppxlib
   (>= 0.26.0))
  (mdx
   (and
    :with-test
    (>= 2.3.0)))
  (gospel
   (= 0.3.0))
  (qcheck-stm
   (and
    :with-test
    (and
     :with-test
     (>= 0.5))))
  (ortac-core
   (= :version))
  (ortac-runtime-qcheck-stm
   (= :version)))
 (conflicts
  (result
   (< 1.5))))

(package
 (name ortac-dune)
 (synopsis "Generate dune rules for other ortac plugins")
 (description "Generate dune rules for other ortac plugins")
 (authors
  "Nicolas Osborne <nicolas.osborne@tarides.com>"
  "Charlène Gros <charlène@tarides.com>")
 (depends
  (ocaml
   (>= 4.12.0))
  fmt
  (cmdliner
   (>= 1.1.0))
  (ortac-core
   (= :version))
  (ortac-qcheck-stm
   (and
    :with-test
    (= :version)))
  (ortac-wrapper
   (and
    :with-test
    (= :version)))))

(package
 (name ortac-runtime-qcheck-stm)
 (synopsis "Runtime support library for Ortac/QCheck-STM-generated code")
 (description "")
 (authors
  "Nicolas Osborne <nicolas.osborne@tarides.com>"
  "Nikolaus Huber <mail@n-huber.eu>")
 (depends
  (qcheck-multicoretests-util
   (>= 0.8))
  (qcheck-core
   (>= 0.25))
  (qcheck-multicoretests-util
   (>= 0.8))
  (qcheck-core
   (>= 0.25))
  (ocaml
   (>= 4.12.0))
  (qcheck-stm
   (>= 0.5))
  (ortac-runtime
   (= :version))))

(package
 (name ortac-examples)
 (synopsis "Ortac examples on different libraries")
 (authors
  "Nicolas Osborne <nicolas.osborne@tarides.com>"
  "Samuel Hym <samuel@tarides.com>")
 (depends
  (qcheck-stm
   (and
    (>= 0.8)
    :with-test))
  (qcheck-multicoretests-util
   (and
    (>= 0.8)
    :with-test))
  (qcheck-stm
   (and
    (>= 0.8)
    :with-test))
  (qcheck-multicoretests-util
   (and
    (>= 0.8)
    :with-test))
  (ocaml
   (>= 4.12.0))
  (ortac-core
   (= :version))
  (ortac-qcheck-stm
   (= :version))
  (ortac-dune
   (= :version))
  (lwt-dllist
   (>= 1.1.0))
  (varray
   (>= 0.2))))