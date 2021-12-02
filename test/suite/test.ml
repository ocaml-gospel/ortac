let () =
  Fmt.(set_style_renderer stderr `Ansi_tty);
  Alcotest.run "Gospel-rtac"
    [
      Arrays.suite;
      Arith.suite;
      Exceptions.suite;
      Terms.suite;
      Types.suite;
      Translation.suite;
    ]
