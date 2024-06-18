{ pkgs ? import <nixpkgs> {}
, nix-ada ? import ./default.nix {}
}:

pkgs.mkShell {
   nativeBuildInputs = [ nix-ada.gnatstudio nix-ada.polyorb ];
   buildInputs = [ nix-ada.florist nix-ada.wayland-ada nix-ada.polyorb nix-ada.dbus-ada ];
}
