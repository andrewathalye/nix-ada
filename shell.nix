{ nix-ada ? import ./default.nix {} }:

nix-ada.pkgs.mkShell {
#   nativeBuildInputs = [ nix-ada.gnatstudio nix-ada.polyorb ];
#   buildInputs = [ nix-ada.florist nix-ada.wayland-ada nix-ada.polyorb nix-ada.dbus-ada ];
}
