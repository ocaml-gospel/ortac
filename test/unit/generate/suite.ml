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

  let variant =
    Ortac_core.Types.Comparison.derive Types.variant |> Result.get_ok
end

module Tests = struct
  let int =
    [%stri
      let cmp_int () =
        Alcotest.(check (list int))
          "compare two int"
          [
            [%e Comparison.int] 1 1;
            [%e Comparison.int] 1 2;
            [%e Comparison.int] 2 1;
          ]
          [ 0; -1; 1 ]]

  let tuple =
    [%stri
      let cmp_tuple () =
        Alcotest.(check (list int))
          "compare two tuples"
          [
            [%e Comparison.tuple] (1, 2, "3") (1, 2, "3");
            [%e Comparison.tuple] (1, 1, "3") (1, 2, "3");
            [%e Comparison.tuple] (1, 2, "4") (1, 2, "3");
          ]
          [ 0; -1; 1 ]]

  let array =
    [%stri
      let cmp_array () =
        Alcotest.(check (list int))
          "compare two arrays"
          [
            [%e Comparison.array] [| 1; 2; 3 |] [| 1; 2; 3 |];
            [%e Comparison.array] [| 1; 2; 3 |] [| 2 |];
            [%e Comparison.array] [| 1; 2; 3 |] [| 0 |];
          ]
          [ 0; -1; 1 ]]

  let record =
    [%stri
      let cmp_record () =
        Alcotest.(check (list int))
          "compare two records"
          [
            [%e Comparison.record] { f0 = 0; f1 = "hello" }
              { f0 = 0; f1 = "hello" };
            [%e Comparison.record] { f0 = 0; f1 = "hello" }
              { f0 = 0; f1 = "world" };
            [%e Comparison.record] { f0 = 0; f1 = "world" }
              { f0 = 0; f1 = "hello" };
          ]
          [ 0; -1; 1 ]]

  let variant =
    [%stri
      let cmp_variant () =
        Alcotest.(check (list int))
          "compare two variants"
          [
            [%e Comparison.variant] (C0 (0, "hello")) (C0 (0, "hello"));
            [%e Comparison.variant]
              (C0 (0, "hello"))
              (C1 { c1a = 0; c1b = "hello" });
            [%e Comparison.variant] C2 (C0 (0, "hello"));
          ]
          [ 0; -1; 1 ]]

  let tests = [ int; tuple; array; record; variant ]
end

let suite =
  [
    [%stri
      let suite =
        ( "Comparison",
          [
            ("compare int is ok", `Quick, cmp_int);
            ("compare tuple is ok", `Quick, cmp_tuple);
            ("compare array is ok", `Quick, cmp_array);
            ("compare record is ok", `Quick, cmp_record);
            ("compare variant is ok", `Quick, cmp_variant);
          ] )];
  ]

let () =
  Fmt.pf Fmt.stdout "%a@." Ppxlib_ast.Pprintast.structure
    (Types.types @ Tests.tests @ suite)
