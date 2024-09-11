In this file, we test the different ways to make the `ortac qcheck-stm`
command-line fail, so we load only the `qcheck-stm` plugin:

  $ export ORTAC_ONLY_PLUGIN=qcheck-stm

We can forget to write the configuration file:

  $ cat > foo.mli << EOF
  > type 'a t
  > val make : 'a -> 'a t
  > EOF
  $ ortac qcheck-stm foo.mli foo_config.ml
  Error: Missing configuration file foo_config.ml.

We can give a pair as SUT type:

  $ cat > foo_config.ml << EOF
  > open Foo
  > let init_sut = make 42
  > type sut = int * bool
  > EOF
  $ ortac qcheck-stm foo.mli foo_config.ml
  Error: Unsupported SUT type (int * bool): SUT type must be a type
         constructor, possibly applied to type arguments.

We can give a functional argument to the SUT type:

  $ cat > foo_config.ml << EOF
  > open Foo
  > let init_sut = make 42
  > type sut = (int -> bool) t
  > EOF
  $ ortac qcheck-stm foo.mli foo_config.ml
  Error: Unsupported type parameter int -> bool: only constructors and tuples
         are supported in arguments for the SUT type.

We can give a type that does not exist in the module as the system under test:

  $ cat > foo_config.ml << EOF
  > open Foo
  > let init_sut = make 42
  > type sut = ty
  > EOF
  $ ortac qcheck-stm foo.mli foo_config.ml
  Error: Type ty not declared in the module.

Or forget its argument:

  $ cat > foo_config.ml << EOF
  > open Foo
  > let init_sut = make 42
  > type sut = t
  > EOF
  $ ortac qcheck-stm foo.mli foo_config.ml
  File "foo.mli", line 1, characters 0-9:
  1 | type 'a t
      ^^^^^^^^^
  Error: Incompatible declaration of SUT type: the declaration of the SUT type
         is incompatible with the configured one: t.
  File "foo.mli", line 1, characters 0-9:
  1 | type 'a t
      ^^^^^^^^^
  Error: Missing specification for the SUT type t.

We can forget to instantiate the type parameter of the system under test:

  $ cat > foo_config.ml << EOF
  > open Foo
  > let init_sut = make 42
  > type sut = 'a t
  > EOF
  $ ortac qcheck-stm foo.mli foo_config.ml
  Error: Unsupported type parameter 'a: SUT type should be fully instantiated.

We can forget to specify the type of the system under test (which is already
the case in `foo.mli`):

  $ cat > foo_config.ml << EOF
  > open Foo
  > let init_sut = make 42
  > type sut = int t
  > EOF
  $ ortac qcheck-stm foo.mli foo_config.ml
  File "foo.mli", line 1, characters 0-9:
  1 | type 'a t
      ^^^^^^^^^
  Error: Missing specification for the SUT type t.

Or specify it without any model:

  $ cat > foo.mli << EOF
  > type 'a t
  > (*@ ephemeral *)
  > EOF
  $ ortac qcheck-stm foo.mli foo_config.ml
  File "foo.mli", line 2, characters 3-14:
  2 | (*@ ephemeral *)
         ^^^^^^^^^^^
  Error: Missing model(s) for the SUT type t.

We can give a non-existing function for `init_state`:

  $ cat > foo.mli << EOF
  > type 'a t
  > (*@ mutable model value : 'a *)
  > EOF
  $ ortac qcheck-stm foo.mli foo_config.ml
  Error: Function make not declared in the module.

We can forget to specify the function used for the `init_state` function:

  $ cat > foo.mli << EOF
  > type 'a t
  > (*@ mutable model value : 'a list *)
  > val make : 'a -> 'a t
  > EOF
  $ ortac qcheck-stm foo.mli foo_config.ml
  File "foo.mli", line 3, characters 0-21:
  3 | val make : 'a -> 'a t
      ^^^^^^^^^^^^^^^^^^^^^
  Error: Unsupported INIT function make: the function called in the INIT
         expression must be specified to initialize the model state.

Or specify it in a manner that does not allow to deduce the complete `state`:

  $ cat > foo.mli << EOF
  > type 'a t
  > (*@ mutable model value : 'a list
  >     model size : integer
  >     model max_size : integer *)
  > val make : 'a -> 'a t
  > (*@ t = make a
  >     requires true
  >     ensures t.max_size = 123 *)
  > EOF
  $ ortac qcheck-stm foo.mli foo_config.ml
  File "foo.mli", line 6, characters 3-62:
  6 | ... t = make a
  7 |     requires true
  8 |     ensures t.max_size = 123 ..
  Error: Unsupported INIT function make: the specification of the function
         called in the INIT expression does not specify the following fields of
         the model: value, size.

Or specify it using clauses that cannot be executed:

  $ cat > foo.mli << EOF
  > type 'a t
  > (*@ mutable model value : 'a list *)
  > val make : 'a -> 'a t
  > (*@ t = make a
  >     requires true
  >     ensures t.value = if forall i. i = i then a :: [] else [] *)
  > val dummy : 'a t -> 'a list
  > (*@ l = dummy t
  >     ensures l = t.value *)
  > EOF
  $ ortac qcheck-stm foo.mli foo_config.ml
  Error: Unsupported INIT function: the specification of the function called in
         the INIT expression does not provide a translatable specification for
         the following field of the model: value.
  Warning: Skipping make: model value is declared as modified by the function
           but no suitable ensures clause was found. Specifications should
           contain at least one "ensures x.value = expr" where x is the SUT and
           expr can refer to the SUT only under an old operator and can't refer
           to the returned value.
  File "foo.mli", line 6, characters 25-40:
  6 |     ensures t.value = if forall i. i = i then a :: [] else [] *)
                               ^^^^^^^^^^^^^^^
  Warning: Skipping clause: unsupported quantification.
  File "foo.mli", line 6, characters 25-40:
  6 |     ensures t.value = if forall i. i = i then a :: [] else [] *)
                               ^^^^^^^^^^^^^^^
  Warning: Skipping clause: unsupported quantification.

Or we can give a function that does not return the type of the system under test:

  $ cat > foo.mli << EOF
  > type 'a t
  > (*@ mutable model value : 'a *)
  > val make : int -> unit
  > (*@ make i
  >     modifies () *)
  > EOF
  $ ortac qcheck-stm foo.mli foo_config.ml
  File "foo.mli", line 3, characters 0-52:
  3 | val make : int -> unit
  4 | (*@ make i
  5 |     modifies () *)
  Error: Unsupported INIT expression make: the function called in the INIT
         expression must return a value of SUT type.

We are expected to give a function call as expression for the `init_state` function:

  $ cat > foo.mli << EOF
  > type 'a t
  > (*@ mutable model value : 'a *)
  > EOF
  $ cat > foo_config.ml << EOF
  > open Foo
  > let init_sut = 42
  > type sut = int t
  > EOF
  $ ortac qcheck-stm foo.mli foo_config.ml
  File "foo_config.ml", line 2, characters 15-17:
  2 | let init_sut = 42
                     ^^
  Error: Unsupported INIT expression 42: the INIT expression is expected to be
         a function call (the specification of that function is required to
         initialize the model state).

It also does not support qualified names:

  $ cat > foo.mli << EOF
  > type 'a t
  > (*@ mutable model value : 'a *)
  > EOF
  $ cat > foo_config.ml << EOF
  > let init_sut = Foo.make 42
  > type sut = int t
  > EOF
  $ ortac qcheck-stm foo.mli foo_config.ml
  Error: Unsupported INIT function Foo.make: qualified names are not yet
         supported.

It checks the number of arguments in the function call:

  $ cat > foo.mli << EOF
  > type 'a t
  > (*@ mutable model value : 'a *)
  > val make : 'a -> 'a t
  > (*@ t = make a
  >     ensures t.value = a *)
  > EOF
  $ cat > foo_config.ml << EOF
  > open Foo
  > let init_sut = make 42 73
  > type sut = int t
  > EOF
  $ ortac qcheck-stm foo.mli foo_config.ml
  File "foo_config.ml", line 2, characters 15-25:
  2 | let init_sut = make 42 73
                     ^^^^^^^^^^
  Error: Error in INIT expression make 42 73: mismatch in the number of
         arguments between the INIT expression and the function specification.

We shouldn't be able to define a model by itsef in the `make` function:
  $ cat > foo.mli << EOF
  > type 'a t
  > (*@ mutable model value : 'a list *)
  > val make : 'a -> 'a t
  > (*@ t = make a
  >     requires true
  >     ensures t.value = t.value *)
  > val dummy : 'a t -> 'a list 
  > (*@ l = dummy t 
  >     ensures l = t.value *)
  > EOF
  $ cat > foo_config.ml << EOF
  > open Foo
  > let init_sut = make 42
  > type sut = int t
  > EOF
  $ ortac qcheck-stm foo.mli foo_config.ml
  Error: Unsupported INIT function: the specification of the function called in
         the INIT expression does not provide a translatable specification for
         the following field of the model: value.
  Warning: Skipping make: model value is declared as modified by the function
           but no suitable ensures clause was found. Specifications should
           contain at least one "ensures x.value = expr" where x is the SUT and
           expr can refer to the SUT only under an old operator and can't refer
           to the returned value.
  File "foo.mli", line 6, characters 22-23:
  6 |     ensures t.value = t.value *)
                            ^
  Warning: Skipping clause: impossible to define the initial value of the model
           with a recursive expression.

If we add some custom generators, we should do so in a `Gen` module that is implemented (functor application or reference to another module are not supported):
  $ cat > foo.mli << EOF
  > type 'a t
  > (*@ mutable model value : 'a *)
  > val make : 'a -> 'a t
  > (*@ t = make a
  >     ensures t.value = a *)
  > EOF
  $ cat > foo_config.ml << EOF
  > module Gen = QCheck.Gen
  > open Foo
  > let init_sut = make 42
  > type sut = int t
  > EOF
  $ ortac qcheck-stm foo.mli foo_config.ml
  Error: Unsupported Gen module definition: only structures are allowed as
         module definition here.
