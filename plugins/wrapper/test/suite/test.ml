let () =
  Fmt.(set_style_renderer stderr `Ansi_tty);
  Alcotest.run "Ortac"
    [
      Arrays.suite;
      Arith.suite;
      Exceptions.suite;
      Terms.suite;
      Translation.suite;
    ]
