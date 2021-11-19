let translate path =
  let module_name = Ortac_core__Utils.module_name_of_path path in
  Gospel.Parser_frontend.parse_ocaml_gospel path
  |> Ortac_core__Utils.type_check [] path
  |> fun (env, sigs) ->
  assert (List.length env = 1);
  let driver = Ortac_core.Drv.init module_name (List.hd env) in
  Ortac_core.Translate.signature ~driver sigs

let mutability =
  let open Ortac_core.Translated in
  let pp_mut ppf (m : mutability) =
    match m with
    | Unknown -> Fmt.pf ppf "Unknown"
    | Immutable -> Fmt.pf ppf "Immutable"
    | Mutable -> Fmt.pf ppf "Mutable"
    | Dependant _ -> Fmt.pf ppf "Dependant"
  in
  let eq m n =
    match (m, n) with
    | Unknown, Unknown
    | Immutable, Immutable
    | Mutable, Mutable
    | Dependant _, Dependant _ ->
        true
    | _ -> false
  in
  Alcotest.testable pp_mut eq

let get_mutability = function
  | Ortac_core.Translated.Type t -> t.mutable_
  | _ -> assert false

let is_pure = function
  | Ortac_core.Translated.Value v -> v.pure
  | _ -> assert false

let type_name = function
  | Ortac_core.Translated.Type t -> t.name
  | _ -> assert false

let val_name = function
  | Ortac_core.Translated.Value v -> v.name
  | _ -> assert false

let type_unknown () =
  let translations = translate "./translation/unknown.mli" in
  Ortac_core.Drv.iter_translation
    ~f:(fun t ->
      print_endline (type_name t);
      Alcotest.(check mutability)
        (Fmt.str "%s is unknown" (type_name t))
        Ortac_core.Translated.Unknown (get_mutability t))
    translations

let type_mutability () =
  let translations = translate "./translation/mutable.mli" in
  Ortac_core.Drv.iter_translation
    ~f:(fun t ->
      Alcotest.(check mutability)
        (Fmt.str "%s is mutable" (type_name t))
        Ortac_core.Translated.Mutable (get_mutability t))
    translations

let type_immutability () =
  let translations = translate "./translation/immutable.mli" in
  Ortac_core.Drv.iter_translation
    ~f:(fun t ->
      Alcotest.(check mutability)
        (Fmt.str "%s is immutable" (type_name t))
        Ortac_core.Translated.Immutable (get_mutability t))
    translations

let type_dependant () =
  let translations = translate "./translation/dependant.mli" in
  Ortac_core.Drv.iter_translation
    ~f:(fun t ->
      Alcotest.(check mutability)
        (Fmt.str "%s is dependant" (type_name t))
        (Ortac_core.Translated.Dependant
           (fun _ -> Ortac_core.Translated.Unknown))
        (get_mutability t))
    translations

let val_pure () =
  let translations = translate "./translation/pure.mli" in
  Ortac_core.Drv.iter_translation
    ~f:(fun v ->
      Alcotest.(check bool) (Fmt.str "%s is pure" (val_name v)) true (is_pure v))
    translations

let suite =
  ( "Translation",
    [
      ("type mutability", `Quick, type_mutability);
      ("type immutability", `Quick, type_immutability);
      ("type unknown", `Quick, type_unknown);
      ("value purity", `Quick, val_pure);
    ] )
