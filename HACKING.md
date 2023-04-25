# Developer documentation

## How to add a plugin

`ortac` has a plugin architecture in order to add new ways to use the `gospel`
to OCaml translation. Basically, a plugin brings a new subcommand to the
`ortac` command line interface.

In order to let the user easily write a plugin:

- `ortac.ortac_core` is provided as a library for translating `gospel` terms
  into OCaml expressions
- `ortac.register` library exposes the `Registration` module that contains:
    + a function `register` to let the writer registers the plugin.
    + some utility functions to help the writer wrapping what is done with the
      translation into a `unit Cmdliner.Cmd.t`.
