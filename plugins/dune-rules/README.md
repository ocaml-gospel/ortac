# Dune-rules plugin for Ortac

This directory contains a plugin for [Ortac] that can generate the dune rules
for the other plugins.

[Ortac]: ../../README.md

## Installation

Follow the global [installation instructions] in the main README of
this repository. The dune-rules plugin is provided by the OPAM package
`ortac-dune.opam`.

[installation instructions]: ../../README.md#installation

## Quick start

The dune-rules plugin can be used to generate dune rules for other ortac
plugins (the qcheck-stm plugin for now). You have to give it the option you want to
pass to the other plugins and some more information for dune.

Let's say you want use the [Ortac/QCheck-STM] plugin on a module interface
`lib.mli` to generate QCheck-STM tests for this module using `make 42 'a'` as
initial value of type `char t`, including the `Lib_incl` module, in the context
of the `pack` package.

Then you can run:

```shell
$ ortac dune qcheck-stm lib "make 42 'a'" "char t" lib_tests --include=lib_incl --package=pack --with-stdout-to=dune.inc
```

to generate the dune rules to generate and run the tests. Next time you run
`dune runtest`, your code should be tested using QCheck-STM.

[Ortac/QCheck-STM]: ../qcheck-stm/README.md

## Supported plugins

- `qcheck-stm`
