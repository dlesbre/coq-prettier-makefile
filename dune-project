(lang dune 3.6)

(name coqprettiermakefile)

(generate_opam_files true)

(source
 (github dlesbre/coq-prettier-makefile))

(authors "Dorian Lesbre")

(maintainers "Dorian Lesbre")

(license LICENSE)

; (documentation https://url/to/documentation)

(package
 (name coqprettiermakefile)
 (synopsis
  "A small program to prettify and colorize the output of coq makefile")
 (description "")
 (depends ocaml dune ANSITerminal)
 (tags
  (coq makefile coq-makefile cli)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
