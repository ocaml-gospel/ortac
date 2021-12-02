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

let mutability_to_string = function
  | Ortac_core.Translated.Mutable -> "mutable"
  | Ortac_core.Translated.Unknown -> "unknown"
  | Ortac_core.Translated.Dependant _ -> "dependant"
  | Ortac_core.Translated.Immutable -> "immutable"

let is_val (t : Ortac_core.Translated.structure_item) =
  match t with Ortac_core.Translated.Value _ -> true | _ -> false

let test_mutability path mut flag () =
  let translations = translate path in
  Ortac_core.Drv.iter_translation
    ~f:(fun t ->
      if !flag then (
        print_endline (type_name t);
        Alcotest.(check mutability)
          (Fmt.str "%s is %s" (type_name t) (mutability_to_string mut))
          mut (get_mutability t))
      else if is_val t then flag := true)
    translations

let type_unknown () =
  let flag = ref false in
  test_mutability "./translation/unknown.mli" Ortac_core.Translated.Unknown flag

let type_mutable () =
  let flag = ref false in
  test_mutability "./translation/mutable.mli" Ortac_core.Translated.Mutable flag

let type_immutable () =
  let flag = ref false in
  test_mutability "./translation/immutable.mli" Ortac_core.Translated.Immutable
    flag

let type_dependant () =
  let flag = ref false in
  test_mutability "./translation/dependant.mli"
    (Ortac_core.Translated.Dependant (fun _ -> Ortac_core.Translated.Unknown))
    flag

let val_pure () =
  let translations = translate "./translation/pure.mli" in
  Ortac_core.Drv.iter_translation
    ~f:(fun v ->
      Alcotest.(check bool) (Fmt.str "%s is pure" (val_name v)) true (is_pure v))
    translations

let suite =
  ( "Translation",
    [
      ("type mutable", `Quick, type_mutable ());
      ("type immutable", `Quick, type_immutable ());
      ("type unknown", `Quick, type_unknown ());
      ("type dependant", `Quick, type_dependant ());
      ("value purity", `Quick, val_pure);
    ] )
