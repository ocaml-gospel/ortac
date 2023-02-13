<div align="center">
  <h1>Ortac's frontend for Monolith</h1>
</div>


<div align="center">

  :warning: **Disclamer:** This project is still experimental. 
  No support will be provided at this point, and its behaviour is still unstable.

</div>

## About

This frontend makes Ortac generate a [Monolith](https://gitlab.inria.fr/fpottier/monolith) program ready to be fuzzed.

There are some limitations the user should know about:

- as Monolith does not support tuple greater than pairs, this frontend does not either
- the generated data generators are not very smart, so if you have strict preconditions or invariants, Monolith of afl-fuzz will generate a lot of uninformative inputs.

## Getting Started

### Installation

This is part of the `ortac` package. So, you just have to follow the instructions 
[here](https://github.com/ocaml-gospel/ortac#installation). 

This frontend depends on a special runtime that should be installed with `ortac`.

It also depends on Monolith. You can find the installation process 
[here](https://gitlab.inria.fr/fpottier/monolith#installation).

### Usage

In order to generate the Monolith program, call `ortac` with the frontend option set to `monolith`
and redirect the result to a file of your choosing:

```shell
$ ortac --frontend=monolith <file.mli> > main.ml
```

In order to compile this Monolith program, you'll need some depedencies. The easiest way to go is to
use a Dune file:

```dune
(executable
  (name main)
  (libraries ortac_runtime_monolith monolith))
```

Then you can compile the Monolith program:

```shell
$ dune build
```

From there, you can either use the program for random testing:

```shell
$ ./path/to/main.exe
```
or, if you have compiled with afl instrumentation, you can use afl-fuzz:

```shell
$ mkdir outputs
$ mkdir intputs
$ echo "some intput" > inputs/input
$ afl-fuzz -i inputs/ -o outputs/ -- ./path/to/main.exe @@
```
