{ pkgs ? import <nixpkgs> {}
, nix ? import ./default.nix {} }:
  pkgs.mkShell {
     buildInputs = [ nix.gnatstudio ];
}
