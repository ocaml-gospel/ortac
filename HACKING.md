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

## How to prepare the repository for a new release

Beware that the `opam` files are generated from the `dune-project` one. No
hand-made changes in the former will survive the CI.

The choice has been made that all the packages have the same version number and
are released at the same time, even the ones that didn't see any changes since
the previous release.

In order to update the version number, one needs to update the `version` field
in the `dune-project` file, run `dune runtest --auto-promote` and commit the
changes.

Update the Changelog with the new version number.

Add new authors in the `dune-project` file and update the LICENSE file if
needed (new authors and/or change of year).

We try to have a contextual summary of the Changelog as release note (attached
both to the git tag and the github release).
