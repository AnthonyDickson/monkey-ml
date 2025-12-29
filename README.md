# monkeylang

An OCaml implementation of Monkey Language from "Writing an Interpreter in Go" by Thorsten Ball

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
