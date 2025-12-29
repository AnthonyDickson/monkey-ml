(lang dune 3.19)

(name monkeylang)

(generate_opam_files true)

(source
 (uri forgejo.mystash.dev/anthony/monkeylang.git))

(authors "Anthony Dickson <anthony.dickson9656@gmail.com>")

(maintainers "Anthony Dickson <anthony.dickson9656@gmail.com>")

(license MIT)

(documentation https://url/to/documentation)

(package
 (name monkeylang)
 (synopsis "An interpreter for MonkeyLang")
 (description
  "An interpreter for MonkeyLang, the language from \"Writing an Interpreter in Go\" by Thorsten Ball")
 (depends ocaml)
 (tags ("interpreter")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
