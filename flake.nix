{
  description = "OCaml Dev Environment";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs =
    { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        devShells.default =
          with pkgs;
          mkShell {
            packages = with pkgs; [
              ocaml
              ocamlPackages.utop
              ocamlPackages.dune_3
              ocamlPackages.odoc
              ocamlPackages.ocaml-lsp
              ocamlPackages.ocamlformat
              ocamlPackages.alcotest
              # For formatting markdown
              dprint
            ];
          };
      }
    );
}
