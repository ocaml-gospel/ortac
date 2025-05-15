# Unreleased

- [Wrapper] Reorganise tests into separate files
  [\#306](https://github.com/ocaml-gospel/ortac/pull/306)
- [Wrapper] Add support for old operator in the wrapper plugin
  [\#297](https://github.com/ocaml-gospel/ortac/pull/297)
- [Wrapper] Extend Wrapper plugin tests
  [\#299](https://github.com/ocaml-gospel/ortac/pull/299)
- [Wrapper] Fix incorrect error type for violated preconditions and
  postconditions
  [\#295](https://github.com/ocaml-gospel/ortac/pull/295)

# 0.6.1

- [QCheck-STM] Fix labelled arguments in runnable scenario
  [\#302](https://github.com/ocaml-gospel/ortac/pull/300)
- Fix `lwt_dllist_spec` signature in `/examples` to meet the implementation
  [\#301](https://github.com/ocaml-gospel/ortac/pull/301/)
- [QCheck-STM] Modifies SUTs in place in the store
  [\#296](https://github.com/ocaml-gospel/ortac/pull/296)

# 0.6.0

- Make search for returned value description more flexible
  [\#291](https://github.com/ocaml-gospel/ortac/pull/291)
- Fix generation of the `QCheck.Fn.apply` identifier
  [\#288](https://github.com/ocaml-gospel/ortac/pull/288)
- Improve failure message in case of out of domain failure
  [\#287](https://github.com/ocaml-gospel/ortac/pull/287)

# 0.5.0

- Add `submodule` optional argument to Ortac/QCheck-STM and Ortac/Dune
  [\#281](https://github.com/ocaml-gospel/ortac/pull/281)
- Add `module-prefix` optional argument to Ortac/QCheck-STM and Ortac/Dune
  [\#280](https://github.com/ocaml-gospel/ortac/pull/280)
- Add support for generating function values
  [\#277](https://github.com/ocaml-gospel/ortac/pull/277)
- Extend QCheck-STM plugin tests
  [\#271](https://github.com/ocaml-gospel/ortac/pull/271)
- Remove unnecessary stack access from precondition
  [\#264](https://github.com/ocaml-gospel/ortac/pull/264)

# 0.4.0

- Add support for running tests in separate processes with a timeout
  [\#259](https://github.com/ocaml-gospel/ortac/pull/259)
- Add `Reserr.{traverse,traverse_,promote_mapi}`, rename `Reserr.map` to
  `Reserr.promote_map` and remove `Reserr.concat_map`
  [\#250](https://github.com/ocaml-gospel/ortac/pull/250)
- Fix display of the runnable scenario for protected values
  [\#251](https://github.com/ocaml-gospel/ortac/pull/251)
- Add support for returning sut values
  [\#253](https://github.com/ocaml-gospel/ortac/pull/253)
- Add support for multiple sut arguments
  [\#247](https://github.com/ocaml-gospel/ortac/pull/247)
- Fix sut as type argument or inside tuple bug
  [\#245](https://github.com/ocaml-gospel/ortac/pull/245)

# 0.3.0

- Read an optional `cleanup` function from configuration module
  [\#226](https://github.com/ocaml-gospel/ortac/pull/226)
- Fix field access translation
  [\#229](https://github.com/ocaml-gospel/ortac/pull/229)
- Add support for functional type in model
  [\#230](https://github.com/ocaml-gospel/ortac/pull/230)
- Remove bug in qcheck-stm plugin when returning integers
  [\#240](https://github.com/ocaml-gospel/ortac/pull/240)
- Add support for testing functions with tuple arguments/return values
  [\#237](https://github.com/ocaml-gospel/ortac/pull/237)
- Add dune-rules plugin
  [\#190](https://github.com/ocaml-gospel/ortac/pull/190)
  [\#218](https://github.com/ocaml-gospel/ortac/pull/218)
- Add support for testing functions without a sut argument
  [\#235](https://github.com/ocaml-gospel/ortac/pull/235)
- Add error for empty command type
  [\#234](https://github.com/ocaml-gospel/ortac/pull/234)
- Move to a module-based configuration
  [\#214](https://github.com/ocaml-gospel/ortac/pull/214)
- Add support for custom ghost types as model
  [\#228](https://github.com/ocaml-gospel/ortac/pull/228)

# 0.2.0

- Make `--help` and `--version` work even without any plugins installed
  [\#217](https://github.com/ocaml-gospel/ortac/pull/217)
- Improve test-failure message
  [\#202](https://github.com/ocaml-gospel/ortac/pull/202) and
  [\#204](https://github.com/ocaml-gospel/ortac/pull/204) and
  [\#206](https://github.com/ocaml-gospel/ortac/pull/206)
- Add a comment warning that the file is generated
  [\#198](https://github.com/ocaml-gospel/ortac/pull/198)
- Add support for type invariants
  [\#197](https://github.com/ocaml-gospel/ortac/pull/197)
- Add an include option to qcheck-stm cli
  [\#181](https://github.com/ocaml-gospel/ortac/pull/181)
- Add a quiet flag
  [\#179](https://github.com/ocaml-gospel/ortac/pull/179)
- Check for out of scope variables
  [\#175](https://github.com/ocaml-gospel/ortac/pull/175)
- Translate constant integer patterns with a guard testing for equality
  [\#174](https://github.com/ocaml-gospel/ortac/pull/174)

# 0.1.0

- Initial release
