let () =
  Fmt.(set_style_renderer stderr `Ansi_tty);
  Alcotest.run "Gospel-rtac" [ Generated.suite ]
