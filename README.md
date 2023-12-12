<!-- LTeX: language=en -->

# Coq prettier makefile

[![OCaml Versions](https://img.shields.io/badge/ocaml%20version-4.10.x%20--%204.14.x-blue)](./coqprettiermakefile.opam)
[![CI Status](https://img.shields.io/github/actions/workflow/status/dlesbre/coq-prettier-makefile/ocaml.yml?label=CI)](https://github.com/dlesbre/coq-prettier-makefile/actions)
[![License](https://img.shields.io/github/license/dlesbre/coq-prettier-makefile)](./LICENSE)

This is a simple utility script to render a prettier output when compiling large
Coq projects. It prints a nice aligned output with color coded build times and memory usages :

![](render.png)

Also has a feature to pretify error message by adding color and showing the relevant
file location:

![](warning.png)

## Installation and usage:

This package requires ANSITerminal to build:

```console
opam install ANSITerminal
```

To install simply clone this repository and build with `dune`:

```console
git clone git@github.com:dlesbre/coq
cd coq-prettier-makefile
dune build
dune install
```

To use simply call `coq-make` where you would usually call `make`. It will
internally call the makefile, and parse its output to render the display. This
should work even if you use a wrapper makefile around the standard coq_makefile.

Any flag arguments are forwarded to `make`, so you can call `coq-make -j4` with
the expected result. For non flag arguments `coq-make` will try to be smart. If
it looks like a file appearing in the `_CoqProject`, it will call `make
full/file/name.vo`, otherwise it will pass the argument as usual. This
allows typing `coq-make file` instead of the lengthier `coq-make
path/to/file.vo`
