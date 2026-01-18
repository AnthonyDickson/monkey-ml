# Monkey ML

An OCaml implementation of the [Monkey language](https://monkeylang.org) from "Writing an Interpreter in Go" by Thorsten Ball

## Getting Started

Refer to [flake.nix](./flake.nix) for the packages needed to compile this project.
If you have `nix`, you can run `nix develop` to enter a dev shell with all of the dependencies installed.

## Useful Commands

- Interpreter (REPL) with project modules:

  ```shell
  dune utop
  ```

- Build and watch for file changes:

  ```shell
  dune build -w
  ```

- Build, run and watch for file changes:

  ```shell
  dune exec monkeylang -w
  ```

- Run tests and watch for file changes:

  ```shell
  dune test -w
  ```

- Format the entire project:

  ```shell
  dune build @fmt --auto-promote
  ```

## Deviations from the Monkey Canon

Here I list the deviations from the [Monkey Canon](https://monkeylang.org/#the-monkey-canon).

### Integers are not truthy

In the [first book](https://interpreterbook.com/), non-zero integers evaluate to true when used in the conditional of an if exprsession:

```
>>> if (1) { "truthy" } else { "not truthy" }
"truthy"
```

However, in Monkey ML it is an error to use anything other than a boolen expression:

```
>>> if (1) { "truthy" } else { "not truthy" }

            __,__
   .--.  .-"     "-.  .--.
  / .. \/  .-. .-.  \/ .. \
 | |  '|  /   Y   \  |'  | |
 | \   \  \ 0 | 0 /  /   / |
  \ '- ,\.-"""""""-./, -' /
   ''-' /_   ^ ^   _\ '-''
       |  \._   _./  |
       \   \ '~' /   /
        '._ '-=-' _.'
           '-----'

Woops! We ran into some monkey business here!
type mismatch: if (INTEGER)
```

## TODO

- Handle parser error `unexpected prefix token "}", next token is: Eof` for the code:
  ```
  if (true) {
    false
  }
  ```
  Note that `if (true) { false }` works fine
- Ignore empty lines, currently evaluates as null
- Handle EOF, currently prints "Fatal error: exception End_of_file"
- Add alternative to REPL that instead reads from a file and just prints out the result
