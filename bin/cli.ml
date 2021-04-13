let main = function
  | "default" -> Gospel_rtac.Generator.generate
  | "monolith" -> Gospel_rtac_monolith.Generator.generate
  | _ -> raise (failwith "not yet implemented")

open Cmdliner

let ocaml_file =
  let parse s =
    match Sys.file_exists s with
    | true ->
        if Sys.is_directory s (* || Filename.extension s <> ".mli" *) then
          `Error (Printf.sprintf "Error: `%s' is not an OCaml interface file" s)
        else `Ok s
    | false -> `Error (Printf.sprintf "Error: `%s' not found" s)
  in
  Arg.(
    required
    & pos 0 (some (parse, Format.pp_print_string)) None
    & info [] ~docv:"FILE")

let generator =
  let doc = "" in
  Arg.(
    value & opt string "default"
    & info [ "g"; "generator" ] ~docv:"GENERATOR" ~doc)

let cmd =
  let doc = "Run GOSPEL-RTAC." in
  (Term.(const main $ generator $ ocaml_file), Term.info "gospel-rtac" ~doc)

let () = Term.(exit @@ eval cmd)
