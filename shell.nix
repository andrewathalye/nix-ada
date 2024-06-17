{ pkgs ? import <nixpkgs> {}
, nix-ada ? import ./default.nix {}
}:

pkgs.mkShell {
   nativeBuildInputs = [ nix-ada.gnatstudio ];
   buildInputs = [ nix-ada.florist ];
}
