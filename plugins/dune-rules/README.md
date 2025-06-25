# Dune-rules plugin for Ortac

This directory contains a plugin for [Ortac] that can generate the dune rules
for the other plugins.

[Ortac]: ../../README.md

## Installation

Follow the global [installation instructions] in the main README of this
repository. The dune-rules plugin is provided by the OPAM package
`ortac-dune.opam`.

[installation instructions]: ../../README.md#installation

## QCheck-STM - Quick start

The dune-rules plugin can be used to generate dune rules for other ortac
plugins. You have to give it the option you
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

## Wrapper - Quick start

The dune-rules plugin can also generate dune rules for the [Ortac/Wrapper]
plugin.
This plugin generates a wrapped implementation from an interface file.

For instance, to apply the wrapper to a module interface `lib.mli`,
you can use the following dune stanza:

First you should instantiate your library,
```dune
(library
 (name lib)
 (modules lib)
 (package my_pkg))
```

```dune
(rule
 (alias runtest)
 (mode promote)
 (action
  (with-stdout-to
   dune.wrapper.inc
   (setenv
    ORTAC_ONLY_PLUGIN
    dune-rules
   (run ortac dune wrapper lib.mli --package=my_pkg --output=wrapper.ml)))))

(include dune.wrapper.inc)
```

This will generate a `wrapper.ml` file based on `lib.mli`, which has the
same signature. The `--output` is optional. If it is not present,
the output file will, by default, append `__wrapped` to the name of the
given interface.

The generated rules in `dune.wrapper.inc` will first declare the generated module
as part of a library, then copy the original interface to the
target name (or appended with `__wrapped`) to separate the
generated couple of files and users ones. Finally, it runs Ortac with
the wrapper plugin to generate the wrapped OCaml file.

More details about the wrapper can be found in [Ortac/Wrapper].

[Ortac/Wrapper]: ../wrapper/README.md

## Supported plugins

- `qcheck-stm`
- `wrapper`
