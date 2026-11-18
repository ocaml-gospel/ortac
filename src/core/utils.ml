open Gospel_typecheckerlibs

let term_printer text global_loc (t : Tast.term) =
  let open Ppxlib.Location in
  try
    String.sub text
      (t.t_loc.loc_start.pos_cnum - global_loc.loc_start.pos_cnum)
      (t.t_loc.loc_end.pos_cnum - t.t_loc.loc_start.pos_cnum)
  with Invalid_argument _ -> Fmt.str "%a" Tast_printer.term t

let module_name_of_path p =
  let filename = Filename.basename p in
  String.index filename '.' |> String.sub filename 0 |> String.capitalize_ascii

let type_check loadpath filename =
  let env, gospel_files =
    Gospel.typecheck ~verbose:false @@ loadpath @ [ filename ]
  in
  let sigs = List.nth gospel_files @@ (List.length gospel_files - 1) in
  (env, sigs.fdefs)
