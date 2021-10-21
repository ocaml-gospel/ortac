let module_name_of_path p =
  let filename = Filename.basename p in
  String.index filename '.' |> String.sub filename 0 |> String.capitalize_ascii

let type_check load_path name sigs =
  let md = Gospel.Tmodule.init_muc name in
  let penv =
    module_name_of_path name
    |> Gospel.Utils.Sstr.singleton
    |> Gospel.Typing.penv load_path
  in
  let gfile = List.fold_left (Gospel.Typing.type_sig_item penv) md sigs in
  let sigs = Gospel.Tmodule.wrap_up_muc gfile |> fun file -> file.fl_sigs in
  (gfile.muc_import, sigs)
