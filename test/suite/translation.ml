let translate path =
  let module_name = Ortac_core__Utils.module_name_of_path path in
  Gospel.Parser_frontend.parse_ocaml_gospel path
  |> Ortac_core__Utils.type_check [] path
  |> fun (env, sigs) ->
  assert (List.length env = 1);
  let driver = Ortac_core.Drv.init module_name (List.hd env) in
  Ortac_core.Translate.signature ~driver sigs

let is_mutable = function
  | Ortac_core.Translated.Type t -> t.mutable_ = Ortac_core.Translated.Mutable
  | _ -> false

let is_pure = function Ortac_core.Translated.Value v -> v.pure | _ -> false

let type_name = function
  | Ortac_core.Translated.Type t -> t.name
  | _ -> "not found"

let val_name = function
  | Ortac_core.Translated.Value v -> v.name
  | _ -> "not found"

let type_mutability () =
  let translations = translate "./translation/mutable.mli" in
  Ortac_core.Drv.iter_translation
    ~f:(fun t ->
      Alcotest.(check bool)
        (Fmt.str "%s is mutable" (type_name t))
        true (is_mutable t))
    translations

let type_immutability () =
  let translations = translate "./translation/immutable.mli" in
  Ortac_core.Drv.iter_translation
    ~f:(fun t ->
      Alcotest.(check bool)
        (Fmt.str "%s is immutable" (type_name t))
        false (is_mutable t))
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
      ("value purity", `Quick, val_pure);
    ] )
