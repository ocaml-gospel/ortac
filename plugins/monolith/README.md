# Monolith plugin for Ortac

:warning: This project is still under heavy development, with no
stable release yet. Expect rough edges!

This directory contains a plugin for [Ortac] that can generate a
standalone executable using [Monolith] to try to falsify the Gospel
specifications of a module by stress-testing the code.

[Ortac]: ../../README.md
[Monolith]: https://gitlab.inria.fr/fpottier/monolith

## Installation

Follow the global [installation instructions] in the main README of
this repository. The Monolith plugin is split into two OPAM packages:

- `ortac-monolith.opam` which provides the Monolith plugin for the
  `ortac` command-line tool,
- `ortac-runtime-monolith.opam` which provides the support library for
  the code generated with the Monolith plugin.

[installation instructions]: ../../README.md#installation


## Quick start

The Monolith plugin can be used to generate a program using the
[Monolith] library that will try to invalidate the Gospel
specifications using random testing or fuzzing.

Let’s start with a module interface `lib.mli` containing some Gospel
specifications, borrowed from Gospel documentation:

```ocaml
val euclidean_division: int -> int -> int * int
(*@ q, r = euclidean_division x y
    requires y > 0
    ensures  x = q * y + r
    ensures  0 <= r < y *)
```

Given that interface, `ortac monolith lib.mli` will display a program
equivalent to:

```ocaml
open Monolith
module M = Ortac_runtime_monolith

module R = struct
  include Lib
  module Ortac_runtime = Ortac_runtime_monolith

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
end

module C = Lib

let () =
  let spec = M.int ^> M.int ^!> (M.int *** M.int) in
  declare "euclidean_division is Ok" spec R.euclidean_division
    C.euclidean_division

let () = main 100
```

You can then save this module into a file:

```shell
$ ortac monolith lib.mli -o main.ml
```

and compile this into an executable using:

- the library `Lib` itself, obviously,
- the libraries `monolith` and `ortac-runtime-monolith`.

With `dune`, the configuration would then look something like:

```dune
(executable
 (name main)
 (libraries monolith ortac-runtime-monolith))
```

From there, you can either run the program directly for random
testing:

```shell
$ ./path/to/main.exe
```

or, if you are using a compiler with AFL instrumentation, such as the
one installed by

```shell
$ opam switch create 5.0+afl --packages ocaml-variants.5.0.0+options,ocaml-option-afl
```

you can run `afl-fuzz` on the generated program:

```shell
$ mkdir inputs outputs
$ head -c 16 /dev/urandom > inputs/input
$ afl-fuzz -i inputs -o outputs -- ./path/to/main.exe @@
```

## Supported Gospel and other limitations

The Monolith plugin has currently some limitations on what Gospel
specifications are supported. Apart from the general restriction to
the executable fragment of Gospel (as mentioned in the [main README]),
the Monolith plugin does not support yet:

- `model`s,
- the `old` operator.

[main README]: ../../README.md#supported-gospel

There are some other limitations the user should know about:

- as Monolith does not support tuple greater than pairs, this plugin
  does not either,
- the generated data generators are not very smart, so if you have
  strict preconditions or invariants, you will obtain a lot of
  uninformative inputs.
