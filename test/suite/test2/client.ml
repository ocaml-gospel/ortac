open Lib_rtac

let main () =
  let arr = create 10 0 in
  let o = get arr 2 in
  assert (o = 0)

let () =
  Fmt.(set_style_renderer stderr `Ansi_tty);
  try main () with
  | Gospel_runtime.Error e ->
      Fmt.epr "%a" Gospel_runtime.report e;
      exit 125
  | e ->
      Fmt.epr "Unexpected error.@\n";
      raise e
