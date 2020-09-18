(lang dune 2.7.1)
(name gospel-rtac)
(implicit_transitive_deps false)

(generate_opam_files true)
(source (github pascutto/gospel-rtac))
(maintainers "Clément Pascutto <clement@pascutto.fr>")
(authors "Clément Pascutto <clement@pascutto.fr>")

(package
 (name gospel-rtac)
 (synopsis "Runtime assertion checking based on Gospel")
 (description "\
Runtime assertion checking based on Gospel
")
 (documentation https://pascutto.github.io/gospel-rtac/)
 (depends alcotest fmt logs))
