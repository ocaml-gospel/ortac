In this file, we test the different ways to make the `ortac qcheck-stm`
command-line fail.

We can make a syntax error in either the expression for the `init` function, or
in the type declaration for the sytem under test:
  $ cat > foo.mli << EOF
  > type 'a t
  > val make : 'a -> 'a t
  > EOF
  $ ortac qcheck-stm foo.mli "" "int t"
  Error: `' is not a well formed OCaml expression.
  
  $ ortac qcheck-stm foo.mli "make 42" ""
  Error: `' is not a well formed type expression.
  
We can give a type that does not exist in the module as the system under test:
  $ cat > foo.mli << EOF
  > type 'a t
  > EOF
  $ ortac qcheck-stm foo.mli "()" "ty"
  Error: Type `ty' is not declared in the module.
  
We can forget to instatiate the type parameter of the system under test:
  $ cat > foo.mli << EOF
  > type 'a t
  > val make : 'a -> 'a t
  > EOF
  $ ortac qcheck-stm foo.mli "make 42" "'a t"
  Error: Type parameter `'a' should be instantiated.
  
We can forget to specify the type of the system under test:
  $ cat > foo.mli << EOF
  > type 'a t
  > EOF
  $ ortac qcheck-stm foo.mli "make 42" "int t"
  File "foo.mli", line 1, characters 0-9:
  Error: The type `t' given for the system under test is not specified.
  
Or specify it without any model:
  $ cat > foo.mli << EOF
  > type 'a t
  > (*@ ephemeral *)
  > EOF
  $ ortac qcheck-stm foo.mli "make 42" "int t"
  File "foo.mli", line 2, characters 3-14:
  Error: The type `t' given for the system under test has no models.
  
We can give a non-existing function for `init_state`:
  $ cat > foo.mli << EOF
  > type 'a t
  > (*@ mutable model value : 'a *)
  > EOF
  $ ortac qcheck-stm foo.mli "make 42" "int t"
  Error: Function `make' is not declared in the module.
  
We can forget to specify the function used for the `init_state` function:
  $ cat > foo.mli << EOF
  > type 'a t
  > (*@ mutable model value : 'a list *)
  > val make : 'a -> 'a t
  > EOF
  $ ortac qcheck-stm foo.mli "make 42" "int t"
  File "foo.mli", line 3, characters 0-21:
  Error: The function `make' used for `init_state` is not approriately specified.
  
Or specify it in a manner that does not allow to deduce the value of `state`:
  $ cat > foo.mli << EOF
  > type 'a t
  > (*@ mutable model value : 'a list *)
  > val make : 'a -> 'a t
  > (*@ t = make a
  >     requires true *)
  > EOF
  $ ortac qcheck-stm foo.mli "make 42" "int t"
  File "foo.mli", line 4, characters 3-33:
  Error: The function ` t = make a
      requires true ' used for `init_state` is not approriately specified.
  
Or we van give a function that does not return the type of the system under test:
  $ cat > foo.mli << EOF
  > type 'a t
  > (*@ mutable model value : 'a *)
  > val make : int -> unit
  > (*@ make i
  >     modifies () *)
  > EOF
  $ ortac qcheck-stm foo.mli "make 42" "int t"
  File "foo.mli", line 3, characters 0-109:
  Error: The function `make' used for `init_state` does not return `sut`.
  
We are expected to give a function call as expression for the `init_state` function:
  $ cat > foo.mli << EOF
  > type 'a t
  > (*@ mutable model value : 'a *)
  > EOF
  $ ortac qcheck-stm foo.mli "42" "int t"
  Error: The expression `42
  ' given for `init_state` is not a function call.
  
It also does not support qualified names:
  $ cat > foo.mli << EOF
  > type 'a t
  > (*@ mutable model value : 'a *)
  > EOF
  $ ortac qcheck-stm foo.mli "Bar.make 42" "int t"
  Error: Qualified name (`Bar.make
  ') is not supported yet for generating `init_state`.
  
It checks the number of argument in the function call:
  $ cat > foo.mli << EOF
  > type 'a t
  > (*@ mutable model value : 'a *)
  > val make : 'a -> 'a t
  > (*@ t = make a
  >     ensures t.value = a *)
  > EOF
  $ ortac qcheck-stm foo.mli "make 42 73" "int t"
  Error: Mismatch number of arguments between `make 42 73
  ' and the function specification.
  
