This test file is designed to trigger a failure
in the `ortac wrapper` plugin by declaring
Gospel models without providing the required
projection functions.

We first load the plugin
  $ export ORTAC_ONLY_PLUGIN=wrapper

  $ cat > missing.mli << EOF
  > type t
  > (*@ mutable model m: int
  >     mutable model n: int *)
  > EOF
  $ ortac wrapper -o missing_wrapped.ml missing.mli
  File "missing.mli", line 1, characters 0-59:
  1 | type t
  2 | (*@ mutable model m: int
  3 |     mutable model n: int *)
  Error: The model m has no projection function. It was not translated..File "missing.mli", line 1, characters 0-59:
  1 | type t
  2 | (*@ mutable model m: int
  3 |     mutable model n: int *)
  Error: The model n has no projection function. It was not translated..
