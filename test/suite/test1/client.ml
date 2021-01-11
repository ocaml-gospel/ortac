open Lib_rtac

let main () =
  let (_ : int) = f 0 in
  let (_ : int) = g 0 in
  ()

let () =
  Fmt.(set_style_renderer stderr `Ansi_tty);
  try main () with
  | Gospel_runtime.Error e ->
      Fmt.epr "%a" Gospel_runtime.report e;
      exit 125
  | e ->
      Fmt.epr "Unexpected error.@\n";
      raise e
