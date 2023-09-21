In this file, we test the different ways to make the `ortac qcheck-stm`
command-line fail, so we load only the `qcheck-stm` plugin:

  $ export ORTAC_ONLY_PLUGIN=qcheck-stm

We can make a syntax error in either the expression for the `init` function, or
in the type declaration for the sytem under test:

  $ cat > foo.mli << EOF
  > type 'a t
  > val make : 'a -> 'a t
  > EOF
  $ ortac qcheck-stm foo.mli "" "int t"
  Error: Syntax error in OCaml expression .
  $ ortac qcheck-stm foo.mli "make 42" ""
  Error: Syntax error in type .

We can give a pair as SUT type:

  $ ortac qcheck-stm foo.mli "make 42" "int * bool"
  Error: Unsupported SUT type (int * bool): SUT type must be a type
         constructor, possibly applied to type arguments.

We can give a functional argument to the SUT type:

  $ ortac qcheck-stm foo.mli "make 42" "(int -> bool) t"
  Error: Unsupported type parameter int -> bool: only constructors and tuples
         are supported in arguments for the SUT type.

We can give a type that does not exist in the module as the system under test:

  $ cat > foo.mli << EOF
  > type 'a t
  > EOF
  $ ortac qcheck-stm foo.mli "()" "ty"
  Error: Type ty not declared in the module.

We can forget to instantiate the type parameter of the system under test:

  $ cat > foo.mli << EOF
  > type 'a t
  > val make : 'a -> 'a t
  > EOF
  $ ortac qcheck-stm foo.mli "make 42" "'a t"
  Error: Unsupported type parameter 'a: SUT type should be fully instantiated.

We can forget to specify the type of the system under test:

  $ cat > foo.mli << EOF
  > type 'a t
  > EOF
  $ ortac qcheck-stm foo.mli "make 42" "int t"
  File "foo.mli", line 1, characters 0-9:
  1 | type 'a t
      ^^^^^^^^^
  Error: Missing specification for the SUT type t.

Or specify it without any model:

  $ cat > foo.mli << EOF
  > type 'a t
  > (*@ ephemeral *)
  > EOF
  $ ortac qcheck-stm foo.mli "make 42" "int t"
  File "foo.mli", line 2, characters 3-14:
  2 | (*@ ephemeral *)
         ^^^^^^^^^^^
  Error: Missing model(s) for the SUT type t.

We can give a non-existing function for `init_state`:

  $ cat > foo.mli << EOF
  > type 'a t
  > (*@ mutable model value : 'a *)
  > EOF
  $ ortac qcheck-stm foo.mli "make 42" "int t"
  Error: Function make not declared in the module.

We can forget to specify the function used for the `init_state` function:

  $ cat > foo.mli << EOF
  > type 'a t
  > (*@ mutable model value : 'a list *)
  > val make : 'a -> 'a t
  > EOF
  $ ortac qcheck-stm foo.mli "make 42" "int t"
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
  $ ortac qcheck-stm foo.mli "make 42" "int t"
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
  > EOF
  $ ortac qcheck-stm foo.mli "make 42" "int t"
  Error: Unsupported INIT function: the specification of the function called in
         the INIT expression does not provide a translatable specification for
         the following field of the model: value.
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
  $ ortac qcheck-stm foo.mli "make 42" "int t"
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
  $ ortac qcheck-stm foo.mli "42" "int t"
  Error: Unsupported INIT expression 42: the INIT expression is expected to be
         a function call (the specification of that function is required to
         initialize the model state).

It also does not support qualified names:

  $ cat > foo.mli << EOF
  > type 'a t
  > (*@ mutable model value : 'a *)
  > EOF
  $ ortac qcheck-stm foo.mli "Bar.make 42" "int t"
  Error: Unsupported INIT function Bar.make: qualified names are not yet
         supported.

It checks the number of arguments in the function call:

  $ cat > foo.mli << EOF
  > type 'a t
  > (*@ mutable model value : 'a *)
  > val make : 'a -> 'a t
  > (*@ t = make a
  >     ensures t.value = a *)
  > EOF
  $ ortac qcheck-stm foo.mli "make 42 73" "int t"
  Error: Error in INIT expression make 42 73: mismatch in the number of
         arguments between the INIT expression and the function specification.
