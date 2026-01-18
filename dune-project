(lang dune 3.19)

(name monkey-ml)

(generate_opam_files true)

(source
 (uri https://github.com/AnthonyDickson/monkey-ml.git))

(authors "Anthony Dickson <anthony.dickson9656@gmail.com>")

(maintainers "Anthony Dickson <anthony.dickson9656@gmail.com>")

(license MIT)

(documentation https://url/to/documentation)

(package
 (name monkey-ml)
 (synopsis "An interpreter for the Monkey language")
 (description
  "An OCaml implementation of Monkey, the programming language from \"Writing an Interpreter in Go\" by Thorsten Ball")
 (depends ocaml)
 (tags ("interpreter")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
