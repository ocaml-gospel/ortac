(lang dune 3.8)
(using dune_site 0.1)
(using mdx 0.4)

(name ortac)

(package
 (name ortac-core)
 (sites
  (lib plugins)))

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
