{ nix-ada ? import ./default.nix {} }:

nix-ada.pkgs.mkShell {
#   nativeBuildInputs = [ nix-ada.gnatstudio nix-ada.polyorb ];
   buildInputs = [ nix-ada.dbus-ada ];
}
