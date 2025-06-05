open Fmt

let stanza k ppf config = pf ppf "@[<v 1>(%a)@]" k config
let stanza_rule k ppf config = pf ppf "%a@." (stanza k) config

let with_target k ppf config =
  let k ppf config = pf ppf "with-stdout-to@;%s@;%a" "%{targets}" k config in
  stanza k ppf config

let setenv var value k ppf =
  let k ppf config = pf ppf "setenv@;%s@;%s@;%a" var value k config in
  stanza k ppf

let action ppf k =
  let k ppf config = pf ppf "action@;%a" k config in
  stanza k ppf

let action_with_env var value ppf k =
  let k ppf = setenv var value k ppf in
  action ppf k

let package s ppf =
  let k ppf _ = pf ppf "package %s" s in
  stanza k ppf

let quiet ppf _ = pf ppf "--quiet"
let rule ppf stanzas = pf ppf "rule@;%a" (concat stanzas)
let test ppf stanzas = pf ppf "test@;%a" (concat stanzas)
let run ppf args = pf ppf "run@;%a" (concat args)
let ortac ppf _ = pf ppf "ortac"
let runtest ppf _ = pf ppf "(alias runtest)"
let promote ppf _ = pf ppf "(mode promote)"
let targets fn ppf config = pf ppf "(targets %s)" @@ fn config
let dep aux ppf config = pf ppf "%%{dep:%a}" aux config

let optional_argument s prj cfg =
  Option.to_list
  @@ Option.map (fun pref ppf _ -> pf ppf "%s=%s" s pref) (prj cfg)
