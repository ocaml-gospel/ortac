# Dune-rules plugin for Ortac

This directory contains a plugin for [Ortac] that can generate the dune rules
for the other plugins.

[Ortac]: ../../README.md

## Installation

Follow the global [installation instructions] in the main README of this
repository. The dune-rules plugin is provided by the OPAM package
`ortac-dune.opam`.

[installation instructions]: ../../README.md#installation

## Quick start

The dune-rules plugin can be used to generate dune rules for other ortac
plugins (the qcheck-stm plugin for now). You have to give it the option you
want to pass to the other plugins and some more information for dune.

Let's say you want use the [Ortac/QCheck-STM] plugin on a module interface
`lib.mli` to generate QCheck-STM tests.

The best way to use Ortac/Dune is in a dune stanza:

```dune
(rule
 (alias runtest)
 (mode promote)
 (action
  (with-stdout-to
   dune.inc
   (run ortac dune qcheck-stm lib.mli))))

(include dune.inc)
```

This stanza assumes that you have written the configuration for
[Ortac/QCheck-STM] in a file named `lib_config.ml` and that the `Lib` module is
part of the `lib` library. It will write the generated tests in
`lib_stm_tests.ml`. If you want more control, you can use the `--config`,
`--library` and `--output` command-line options to give custom names to these
files.

You can use the `--timeout x` command-line option to run each test in a separate
process, which is automatically killed after `x` seconds. This is useful in case
the test may cause a segmentation fault or include non-terminating computations.
More information about this can be found in [Ortac/QCheck-STM].

[Ortac/QCheck-STM]: ../qcheck-stm/README.md

## Supported plugins

- `qcheck-stm`
