{ nix-ada ? import ./default.nix {}
}:

with nix-ada;
pkgs.mkShell {
   buildInputs = [gnatstudio];
}
