open Ppxlib

let loc = Location.none

module Types = struct
  open Ortac_core

  let mutable_ = Translated.Unknown
  let ghost = false
  let type_ = Translated.type_ ~loc ~mutable_ ~ghost
  let int = type_ ~name:"int" ~kind:(Core [])
  let string = type_ ~name:"string" ~kind:(Core [])
  let array = type_ ~name:"array" ~kind:(Core [ int ])
  let list = type_ ~name:"list" ~kind:(Core [ int ])
  let tuple = type_ ~name:"tuple" ~kind:(Tuple [ int; int; string ])
  let ref_ = type_ ~name:"ref" ~kind:(Core [ int ])
  let t = type_ ~name:"t" ~kind:(Synonyms (array, [ string ]))

  let record =
    type_ ~name:"record" ~kind:(Record [ ("f0", int); ("f1", string) ])

  let variant =
    type_ ~name:"variant"
      ~kind:
        (Variant
           [
             ("C0", Translated.Unnamed [ int; string ]);
             ("C1", Translated.Named [ ("c1a", int); ("c1b", string) ]);
             ("C2", Translated.Unnamed []);
           ])

  let types =
    [%str
      type record = { f0 : int; f1 : string }

      type variant =
        | C0 of (int * string)
        | C1 of { c1a : int; c1b : string }
        | C2]
end

module Comparison = struct
  let int = Ortac_core.Types.Comparison.derive Types.int |> Result.get_ok
  let string = Ortac_core.Types.Comparison.derive Types.string |> Result.get_ok
  let array = Ortac_core.Types.Comparison.derive Types.array |> Result.get_ok
  let tuple = Ortac_core.Types.Comparison.derive Types.tuple |> Result.get_ok
  let list = Ortac_core.Types.Comparison.derive Types.list |> Result.get_ok
  let record = Ortac_core.Types.Comparison.derive Types.record |> Result.get_ok
  let ref_ = Ortac_core.Types.Comparison.derive Types.ref_ |> Result.get_ok
  let t = Ortac_core.Types.Comparison.derive Types.t |> Result.get_ok

  let variant =
    Ortac_core.Types.Comparison.derive Types.variant |> Result.get_ok

  module Tests = struct
    let int =
      [%stri
        let cmp_int () =
          Alcotest.(check (list int))
            "compare two int"
            [ [%e int] 1 1; [%e int] 1 2; [%e int] 2 1 ]
            [ 0; -1; 1 ]]

    let tuple =
      [%stri
        let cmp_tuple () =
          Alcotest.(check (list int))
            "compare two tuples"
            [
              [%e tuple] (1, 2, "3") (1, 2, "3");
              [%e tuple] (1, 1, "3") (1, 2, "3");
              [%e tuple] (1, 2, "4") (1, 2, "3");
            ]
            [ 0; -1; 1 ]]

    let array =
      [%stri
        let cmp_array () =
          Alcotest.(check (list int))
            "compare two arrays"
            [
              [%e array] [| 1; 2; 3 |] [| 1; 2; 3 |];
              [%e array] [| 1; 2; 3 |] [| 2 |];
              [%e array] [| 1; 2; 3 |] [| 0 |];
            ]
            [ 0; -1; 1 ]]

    let record =
      [%stri
        let cmp_record () =
          Alcotest.(check (list int))
            "compare two records"
            [
              [%e record] { f0 = 0; f1 = "hello" } { f0 = 0; f1 = "hello" };
              [%e record] { f0 = 0; f1 = "hello" } { f0 = 0; f1 = "world" };
              [%e record] { f0 = 0; f1 = "world" } { f0 = 0; f1 = "hello" };
            ]
            [ 0; -1; 1 ]]

    let variant =
      [%stri
        let cmp_variant () =
          Alcotest.(check (list int))
            "compare two variants"
            [
              [%e variant] (C0 (0, "hello")) (C0 (0, "hello"));
              [%e variant] (C0 (0, "hello")) (C1 { c1a = 0; c1b = "hello" });
              [%e variant] C2 (C0 (0, "hello"));
              [%e variant] C2 C2;
            ]
            [ 0; -1; 1; 0 ]]

    let ref_ =
      [%stri
        let cmp_ref () =
          let a = ref 42 in
          let b = ref 42 in
          let c = ref 73 in
          Alcotest.(check (list int))
            "compare two ref"
            [ [%e ref_] a b; [%e ref_] a c; [%e ref_] c a ]
            [ 0; -1; 1 ]]

    let t =
      [%stri
        let cmp_t () =
          let a = [| "h"; "e"; "l"; "l"; "o" |] in
          let b = [| "w"; "o"; "r"; "l"; "d" |] in
          Alcotest.(check (list int))
            "compare two arrays"
            [ [%e t] a a; [%e t] a b; [%e t] b a ]
            [ 0; -1; 1 ]]

    let tests = [ int; tuple; array; record; variant; ref_; t ]
  end
end

module Equality = struct
  let int = Ortac_core.Types.Equality.derive Types.int |> Result.get_ok
  let string = Ortac_core.Types.Equality.derive Types.string |> Result.get_ok
  let array = Ortac_core.Types.Equality.derive Types.array |> Result.get_ok
  let tuple = Ortac_core.Types.Equality.derive Types.tuple |> Result.get_ok
  let list = Ortac_core.Types.Equality.derive Types.list |> Result.get_ok
  let record = Ortac_core.Types.Equality.derive Types.record |> Result.get_ok
  let variant = Ortac_core.Types.Equality.derive Types.variant |> Result.get_ok
  let ref_ = Ortac_core.Types.Equality.derive Types.ref_ |> Result.get_ok
  let t = Ortac_core.Types.Equality.derive Types.t |> Result.get_ok

  module Tests = struct
    let int =
      [%stri
        let eq_int () =
          Alcotest.(check (list bool))
            "equality between two int"
            [ [%e int] 42 42; [%e int] 42 73 ]
            [ true; false ]]

    let ref_ =
      [%stri
        let eq_ref () =
          let a = ref 42 in
          let b = ref 42 in
          let c = ref 73 in
          Alcotest.(check (list bool))
            "equality between two ref"
            [ [%e ref_] a b; [%e ref_] a c ]
            [ true; false ]]

    let list =
      [%stri
        let eq_list () =
          Alcotest.(check (list bool))
            "equality between two list"
            [
              [%e list] [ 42; 73 ] [ 42; 73 ];
              [%e list] [ 42; 73 ] [ 42; 74 ];
              [%e list] [ 42; 73 ] [ 42 ];
              [%e list] [ 42 ] [ 42; 73 ];
              [%e list] [] [ 42; 73 ];
            ]
            [ true; false; false; false; false ]]

    let array =
      [%stri
        let eq_array () =
          Alcotest.(check (list bool))
            "equality between two list"
            [
              [%e array] [| 42; 73 |] [| 42; 73 |];
              [%e array] [| 42; 73 |] [| 42; 74 |];
              [%e array] [| 42; 73 |] [| 42 |];
              [%e array] [| 42 |] [| 42; 73 |];
              [%e array] [||] [| 42; 73 |];
            ]
            [ true; false; false; false; false ]]

    let tuple =
      [%stri
        let eq_tuple () =
          Alcotest.(check (list bool))
            "equality between two tuple"
            [
              [%e tuple] (42, 73, "hello") (42, 73, "hello");
              [%e tuple] (42, 73, "hello") (42, 73, "world");
            ]
            [ true; false ]]

    let record =
      [%stri
        let eq_record () =
          Alcotest.(check (list bool))
            "compare two records"
            [
              [%e record] { f0 = 0; f1 = "hello" } { f0 = 0; f1 = "hello" };
              [%e record] { f0 = 0; f1 = "hello" } { f0 = 0; f1 = "world" };
            ]
            [ true; false ]]

    let variant =
      [%stri
        let eq_variant () =
          Alcotest.(check (list bool))
            "compare two variants"
            [
              [%e variant] (C0 (0, "hello")) (C0 (0, "hello"));
              [%e variant]
                (C1 { c1a = 0; c1b = "world" })
                (C1 { c1a = 0; c1b = "hello" });
              [%e variant] C2 (C0 (0, "hello"));
              [%e variant] C2 C2;
            ]
            [ true; false; false; true ]]

    let t =
      [%stri
        let eq_t () =
          let a = [| "h"; "e"; "l"; "l"; "o" |] in
          let b = [| "w"; "o"; "r"; "l"; "d" |] in
          Alcotest.(check (list bool))
            "compare two arrays"
            [ [%e t] a a; [%e t] a b; [%e t] b a ]
            [ true; false; false ]]

    let tests = [ int; ref_; list; array; tuple; record; variant; t ]
  end
end

let suite =
  [%str
    let comparison_suite =
      ( "Comparison",
        [
          ("compare int is ok", `Quick, cmp_int);
          ("compare tuple is ok", `Quick, cmp_tuple);
          ("compare array is ok", `Quick, cmp_array);
          ("compare record is ok", `Quick, cmp_record);
          ("compare variant is ok", `Quick, cmp_variant);
          ("compare synonym is ok", `Quick, cmp_t);
          ("compare ref is ok", `Quick, cmp_ref);
        ] )

    let equality_suite =
      ( "Equality",
        [
          ("equality between int is ok", `Quick, eq_int);
          ("equality between list is ok", `Quick, eq_list);
          ("equality between array is ok", `Quick, eq_array);
          ("equality between tuple is ok", `Quick, eq_tuple);
          ("equality between record is ok", `Quick, eq_record);
          ("equality between variant is ok", `Quick, eq_variant);
          ("equality between ref is ok", `Quick, eq_ref);
          ("equality between synonym is ok", `Quick, eq_t);
        ] )]

let () =
  Fmt.pf Fmt.stdout "%a@." Ppxlib_ast.Pprintast.structure
    (Types.types @ Comparison.Tests.tests @ Equality.Tests.tests @ suite)
