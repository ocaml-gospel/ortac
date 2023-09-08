# Wrapper plugin for Ortac

:warning: This project is still under heavy development, with no
stable release yet. Expect rough edges!

This directory contains a plugin for [Ortac] that can generate a _wrapper_
module that will expose the same interface as the original module but
instrumenting all function calls with assertions corresponding to the Gospel
specifications.

[Ortac]: ../../README.md

## Installation

Follow the global [installation instructions] in the main README of
this repository. The wrapper plugin is provided by the OPAM package
`ortac-wrapper.opam`.

[installation instructions]: ../../README.md#installation


## Quick start

The wrapper plugin can be used to generate a _wrapper_ module that
will expose the same interface as the original module but
instrumenting all function calls with assertions corresponding to the
Gospel specifications.

Let’s start with a module interface `lib.mli` containing some Gospel
specifications, borrowed from Gospel documentation:

```ocaml
val euclidean_division: int -> int -> int * int
(*@ q, r = euclidean_division x y
    requires y > 0
    ensures  x = q * y + r
    ensures  0 <= r < y *)
```

Given that interface, `ortac wrapper lib.mli` will display a code
structured as such:

```ocaml
include Lib
module Ortac_runtime = Ortac_runtime
let euclidean_division x y =
  (* check that y > 0 *)
  (* raise an error if the previous check failed *)
  let q, r =
    try euclidean_division x y
    with e ->
      (* raise an error as the specification do not mention exceptions *)
      raise e
  in
  (* check that x = q * y + r *)
  (* check that 0 <= r < y *)
  (* raise an error if one of the previous checks failed *)
  (q, r)
```

In other words, it produces a module exposing the same interface as
`Lib`, so that each function call is instrumented to test the
specification of the function at runtime.

If you want to use those assertions at runtime in your program, you
can do so:

```
$ ortac wrapper lib.mli -o libAsserts.ml
$ cp lib.mli libAsserts.mli
```

add `LibAsserts` to your build and, in all the modules in which you
want to check the specification on every function call, insert at the
very beginning:

```ocaml
module Lib = LibAsserts
```


## Supported Gospel

The wrapper plugin has currently some limitations on what Gospel
specifications are supported. Apart from the general restriction to
the executable fragment of Gospel (as mentioned in the [main README]),
the wrapper plugin does not support yet:

- `model`s,
- the `old` operator.

[main README]: ../../README.md#supported-gospel
