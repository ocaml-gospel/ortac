let () =
  Fmt.(set_style_renderer stderr `Ansi_tty);
  Alcotest.run "Gospel-rtac"
    [ Generated.comparison_suite; Generated.equality_suite ]
