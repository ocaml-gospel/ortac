# Wrapper plugin for Ortac

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

## Advanced specification

The wrapper plugin also supports advanced Gospel features, such as
logical models and `old` statements,
allowing more precise and expressive specifications for abstract types.
Let's consider a toy example of a polymorphic container type with limited capacity:

```ocaml
type 'a t
(*@ model capacity: int
    mutable model contents: 'a list
    with t
    invariant t.capacity > 0
    invariant List.length t.contents <= t.capacity *)
```

Here, we define two models attached to the type `a t`:
`capacity` which is the fixed size of the container and
`contents` which is the mutable list of elements currently stored.

The invariants ensure that the capacity is strictly positive
and the list of contents never exceeds the declared capacity.

We can then specify the behavior of the functions manipulating this type:

```ocaml
val create: int -> 'a t
(*@ t = create c
    requires c > 0
    ensures t.capacity = c
    ensures t.contents = [] *)

val add: 'a t -> 'a -> unit
(*@ add t x
    modifies t.contents
    ensures t.contents = x :: (old t.contents) *)
```

To be able to translate models in OCaml,
the user needs to provide a projection function for each gospel model.

### Defining projection functions

To validate these specifications at runtime
you must provide projection functions that link OCaml values to their Gospel model.

There are two ways to define projection functions:
- Use the same name as the model (e.g., `capacity`).
- Use a different name, annotated with the attribute `@@projection_for` and the name
of its gospel model (e.g., `to_list`).

For our example:

```ocaml
val capacity : 'a t -> int
val to_list : 'a t -> 'a list [@@projection_for contents]
```

Here, there is no ambiguity that `capacity` corresponds to the model `capacity`,
but `to_list` needs to be explicitly declared as the projection for the model `contents`.

These projection functions are mandatory for the wrapper plugin
to instrument the specifications.
If one is not provided, nothing will be generated, and an error will be printed.

[main README]: ../../README.md#supported-gospel

## Supported Gospel

The Wrapper plugin has some limitations on what Gospel specifications
are supported.

It inherits the limitations of the translation from
Gospel to OCaml provided by the core of Ortac, as listed in the
[main README].

[main README]: ../../README.md#supported-gospel
