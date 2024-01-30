{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
    nativeBuildInputs = with pkgs; [ bash 
    ocamlPackages.ocaml 
    ocamlPackages.dune_3 
    ocamlPackages.findlib 
    ocamlPackages.utop 
    ocamlPackages.odoc 
    ocamlPackages.ocaml-lsp 
    ocamlformat

    ocamlPackages.ppx_expect];
  }
