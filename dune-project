(lang dune 3.0)
(using dune_site 0.1)
(name ortac)
(implicit_transitive_deps false)

(package
  (name ortac-core)
  (sites (lib plugins)))

(package
  (name ortac-runtime))

; Wrapper plugin
(package
  (name ortac-wrapper))

; Monolith plugin
(package
  (name ortac-monolith))

(package
  (name ortac-runtime-monolith))

; QCheck-STM plugin
(package
  (name ortac-qcheck-stm))
