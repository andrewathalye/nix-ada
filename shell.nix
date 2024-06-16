{ pkgs ? import <nixpkgs> {}
, nix-ada ? import ./default.nix {}
}:

pkgs.mkShell {
   buildInputs = [ nix-ada.gnatstudio ];
}
