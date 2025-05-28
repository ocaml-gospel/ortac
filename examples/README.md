# `ortac` examples

:warning: These examples use the development branch of `ortac`, you won't have
the same result with the released version. :warning:

This package centralises examples of libraries with their Gospel
specifications. `ortac` is then used to generate code with runtime assertion
checking. This is thought both as a learning ressource and a way to experiment
with the limitations of the `ortac` tool which is still quite young.

Specifications are written in a style that allows the use the `ortac` tool,
specifically the `qcheck-stm` plugin.

## `lwt_dllist` library

This is an example of relatively simple, yet not trivial, container library.
The specifications make use of the Gospel `sequence` type (for more
information, see [its
documentation](https://ocaml-gospel.github.io/gospel/gospel/Gospelstdlib/Sequence/index.html).

Specifications for this library can be found in the edited copy of the
interface `lwt_dllist_spec.mli`. There is one modification of the actual
interface: the type `'a node` is not abstract anymore. This is necessary in
order to extend the `STM.ty` type with a `Node` constructor. For the
typechecker to accept this extension, the implementation of the `node` type
need to be visible.

The STM `postcond` function pattern match on the returned value encapsulated in
a `STM.ty`. As functions `add_l` and `add_r` both return the node that has been
added, we need to extend the `STM.ty` type with an encoding for
`Lwt_dllist.node`. We add this extension in the configuration module for
Ortac/QCheckSTM.

The generated code can be found in `lwt_dllist_tests.ml`. You can run the test
either with `dune runtest examples/` or by directly running the compiled
executable that you will find in your `_build` directory.

Dune rules have been generated using the `dune-rules` plugin, providing the
`dune` subcommand:

```sh
$ ortac dune qcheck-stm lwt_dllist_spec.mli lwt_dllist_config.ml lwt_dllist_tests.ml --package=ortac-examples --with-stdout-to=dune.lwt_dllist.inc
```

## `varray` library

This is an example of a library exposing a Functor. In order to test a Functor,
it is necessary to instantiate it. The
[`varray`](https://github.com/art-w/varray) library already exposes some
intantiations of its Functor. Here, we provide generated tests for two of them.

The signature of the output of the Functor is originally placed in a `ml` file.
`varray_sig.ml` is a copy of this file with some specifications. As Gospel
doesn't process `ml` files, we extract the content of the module signature and
place a copy in two files: `varray_spec.mli` and `varray_circular_spec.mli`,
the former for the module exposed by `varray` as `Varray` and the latter for
the module exposed by `varray` as `Varray.Circular`. The corresponding `ml`
files are just the respective modules included. Note that we keep the two
dune-generated `mli` files hidden in the `_build` folder.

Here again, as for the previous example, we need to include some code. That is
the extension of the `STM.ty` type. But also a `QCheck` generator for the `'a
elt` type, as some functions of the library take arguments of this type. You
can find these extensions in the `varray_config.ml` and
`varray_circular_config.ml` files.

The generated code can be found in the respective `varray*tests.ml` files. They
have been promoted in the source tree in order to be easily accessible for
reading and curiosity purposes, but this is not necessary for testing purposes.

Dune rules have been generated again using the `dune-rules` plugin with the
following command for `varray_spec.mli` and an adapted version for
`varray_circular_spec.mli`:

```sh
$ ortac dune qcheck-stm varray_spec.mli varray_config.ml varray_tests.ml --package=ortac-examples --with-stdout-to=dune.varray.inc
```

## `Braun trees` library

This example demonstrates the use of the Ortac wrapper plugin on a functional
data structure: the [Braun tree](https://cs.uwaterloo.ca/~plragde/flaneries/FICS/Trees.html#%28part._.Braun_trees%29).

We implemented a simple library for Braun trees and specified its interface
using Gospel. We then used Ortac’s wrapper plugin to automatically generate an
`.ml` file (`braun_tree_wrapped.ml`) with the following command. The wrapped
module preserves the original interface but includes runtime checks derived from
the formal specifications.

```sh
ortac wrapper -o braun_tree_wrapped.ml braun_tree.mli
```

The plugin requires only an additional implementation of a projection function
to link the user-defined types with the corresponding Gospel models. In Gospel
specifications in `braun_tree.mli` (see below), the type `'a t` representing
Braun trees is modeled by two abstract values:
`size`, the number of elements in the tree and
`cont`, the list of elements stored in the tree.

```ocaml
type 'a t
(*@ model size: int
    model cont: 'a list *)
```

We must provide a corresponding implementation in the Ortac runtime for each
model field, using the same name as declared in the Gospel specification. In
this case, we need to define a function to compute the number of elements in the
tree (`size`) and a function `'a t -> 'a list` to extract the contents in the
order (`cont`). Note that we use the algorithm from Okasaki
([Three Algorithms on Braun Trees](https://users.cs.northwestern.edu/~robby/courses/395-495-2013-fall/three-algorithms-on-braun-trees.pdf))
to compute the size of a braun tree in time $O(log^2(n))$.

```ocaml
let rec size = function
  | Empty -> 0
  | Tree (l, _, r) ->
      let m = size r in
      1 + (2 * m) + diff l m

let to_list t =
  let rec aux acc t =
    match t with
    | Empty -> acc
    | Tree (Empty, x, _) -> x :: acc
    | Tree (l, x, Empty) -> aux (x :: acc) l
    | Tree (_, x, _) ->
        let nt = tail t in
        aux (x :: acc) nt
  in
  List.rev (aux [] t)

let cont = to_list
```

It can be used with a simple replacement of the original module, enabling
runtime contract checking in any client code. The following artificial example
creates a Braun tree from a list, adds an element at the beginning and another
at the end, then removes both.
This sequence exercises wrapped operations, and any violation of the
corresponding contracts are detected at runtime by the wrapper with the current
implementation of Braun trees (`braun_tree.ml`).

```ocaml
open Braun_tree_wrapped

let () =
  let b = of_list [ 1; 2; 3; 4 ] in
  let b = cons 0 b in
  let b = snoc 5 b in
  let b = tail b in
  let _ = liat b in
  ()
```

This mechanism ensures the implementation yields its specification at runtime
for the exercised functions (`of_list`, `cons`, `snoc`, `tail`, and `liat`).
It does not attempt to prove correctness in all possible cases but dynamically
enforces the contracts. If any contract is violated during execution, a runtime
error is raised.
This approach differs from traditional testing frameworks like `Alcotest`, where
explicit test cases must be written. With the wrapped module generated by Ortac,
the Gospel specifications themselves define the expected behavior.
As a result, any misuse or incorrect implementation is automatically detected
through contract violations during execution without the need to write dedicated
tests.
