(lang dune 3.0)

(name coqprettiermakefile)

(version 0.1.0)

(generate_opam_files true)

(source
 (github dlesbre/coq-prettier-makefile))

(authors "Dorian Lesbre <dorian.lesbre@gmail.com>")

(maintainers "Dorian Lesbre <dorian.lesbre@gmail.com>")

(license LICENSE)

; (documentation https://url/to/documentation)

(package
 (name coqprettiermakefile)
 (synopsis
  "A small program to prettify and colorize the output of coq makefile")
 (description "")
 (depends
  (ocaml
   (and
    (>= 4.10)
    (< 5.0)))
  dune
  ANSITerminal)
 (tags
  (coq makefile coq-makefile cli)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
