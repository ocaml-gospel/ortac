# `ortac` - OCaml Runtime Assertion Checking.

**Disclamer:** This project is still experimental. No support will be provided
at this point, and its behaviour is still unstable.

## Installation

```
opam pin add -y https://github.com/ocaml-gospel/gospel.git
opam pin add -y https://github.com/ocaml-gospel/ortac.git
opam install ortac
```

## how to add a plugin

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
