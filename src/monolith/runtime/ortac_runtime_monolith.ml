open Monolith

let constructible_int = int_within (Gen.semi_open_interval (-420000) 420000)

let int = ifpol constructible_int int

let positive_int = int_within (Gen.int Int.max_int)

let constructible_array g p =
  easily_constructible
    (Monolith.Gen.array (Gen.int Int.max_int) g)
    (Monolith.Print.array p)

let deconstructible_array p = deconstructible (Monolith.Print.array p)
