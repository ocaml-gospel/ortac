# Unreleased

- [Examples] Add Saturn mpsc examples
  [\#360](https://github.com/ocaml-gospel/ortac/pull/360)
- [QCheck-STM] Allow for user defined `arb_cmd`s
  [\#359](https://github.com/ocaml-gospel/ortac/pull/359)
- [QCheck-STM] Allow for user defined frequencies in generated `arb_cmd`
  [\#358](https://github.com/ocaml-gospel/ortac/pull/358)
- [QCheck-STM+Dune] Add count optional argument
  [\#357](https://github.com/ocaml-gospel/ortac/pull/357)
- [Dune] Add optional arguments to control aliases
  [\#354](https://github.com/ocaml-gospel/ortac/pull/354)
- [QCheck-STM] Add domain bug report generation
  [\#346](Https://github.com/ocaml-gospel/ortac/pull/346)
- [QCheck-STM] Make `ortac_show_cmd` look at the models
  [\#353](https://github.com/ocaml-gospel/ortac/pull/353)
- [QCheck-STM] Refactor bug report printing
  [\#345](https://github.com/ocaml-gospel/ortac/pull/345)
- [Dune] Add domain flag to ortac-dune qcheck-stm
  [\#329](https://github.com/ocaml-gospel/ortac/pull/329)
- [QCheck-STM] Add basic runtime for domain testing
  [\#328](https://github.com/ocaml-gospel/ortac/pull/328)
- [QCheck-STM] Refactor runtime to allow for OCaml5-only sub-library
  [\#316](https://github.com/ocaml-gospel/ortac/pull/316)

# 0.7.2

- [Dune] Fix cycle dependency in `Ortac/Dune-rules` for wrapper plugin
  [\#348](https://github.com/ocaml-gospel/ortac/pull/348)

# 0.7.1

- [Dune] Fix dependencies in generated library declaration for Ortac/Wrapper
  [\#342](https://github.com/ocaml-gospel/ortac/pull/342)
- [Dune] Fix dependencies in `ortac-dune`
  [\#340](https://github.com/ocaml-gospel/ortac/pull/340)

# 0.7.0

- [Wrapper] Add a better management of attribute for projection functions
  [\#332](https://github.com/ocaml-gospel/ortac/pull/332)
- [Wrapper] Print projection function as Ir.Projection and Ir.Value
  [\#331](https://github.com/ocaml-gospel/ortac/pull/331)
- [Wrapper] Prevent code generation if required model projection is absent
  [\#327](https://github.com/ocaml-gospel/ortac/pull/327)
- [QCheck-STM] Fix next-state computation for functions specialised for `int`s
  [\#326](https://github.com/ocaml-gospel/ortac/pull/326)
- [Wrapper] Fix naming conflicts between model projections and function arguments
  [\#324](https://github.com/ocaml-gospel/ortac/pull/324)
- [Wrapper] Add header message in generated files with wrapper mode
  [\#322](https://github.com/ocaml-gospel/ortac/pull/322)
- [Dune] Add automatic dune file generation to the wrapper plugin
  [\#314](https://github.com/ocaml-gospel/ortac/pull/314) and
  [\#335](https://github.com/ocaml-gospel/ortac/pull/335)
- [Wrapper] Fix missing model support in preconditions
  [\#313](https://github.com/ocaml-gospel/ortac/pull/313)
- [Wrapper] Add support for models in the wrapper plugin
  [\#305](https://github.com/ocaml-gospel/ortac/pull/305)
- [QCheck-STM] Disable warning 34 in generated code
  [\#307](https://github.com/ocaml-gospel/ortac/pull/307)
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
