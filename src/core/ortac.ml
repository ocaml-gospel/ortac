let signature ~runtime ~module_name namespace s =
  let context = Context.init module_name namespace in
  let translated = Translate.signature ~context s in
  Report.emit_warnings Fmt.stderr translated;
  Generate.structure runtime translated
