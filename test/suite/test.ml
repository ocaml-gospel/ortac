let suite = [ Arrays.suite; Arith.suite; Exceptions.suite; Terms.suite ]

let () =
  Fmt.(set_style_renderer stderr `Ansi_tty);
  Alcotest.run "Gospel-rtac" suite
