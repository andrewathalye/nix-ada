{ nix-ada }:
nix-ada.pkgs.mkShell {
#   nativeBuildInputs = [ nix-ada.gnatstudio nix-ada.polyorb ];
#   nativeBuildInputs =[ nix-ada.gnatcoverage ];
   buildInputs = [ nix-ada.ada-language-server ];
}
