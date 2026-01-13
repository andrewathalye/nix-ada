{ nix-ada }:
nix-ada.pkgs.mkShell {
   nativeBuildInputs = [ nix-ada.gnatstudio ];
#   nativeBuildInputs =[ nix-ada.gnatcoverage ];
#   nativeBuildInputs = [ nix-ada.wayland-ada-scanner ];
}
