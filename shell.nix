{ nix-ada }:
nix-ada.pkgs.mkShell {
#   nativeBuildInputs = [ nix-ada.gnatstudio nix-ada.polyorb ];
#   nativeBuildInputs =[ nix-ada.gnatcoverage ];
   nativeBuildInputs = [ nix-ada.dbus-ada ];
}
