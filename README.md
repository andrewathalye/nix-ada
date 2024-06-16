Nix-Ada
=======

A set of Nix packages for Ada libraries and applications.

Of note:
libadalang
libadalang-tools
ada-language-server
wayland-ada
gnatstudio

Flake Info
----------
This repository provides a `flake.nix` file, which you may use if you so desire.

It is not necessary, however, and you can also import `default.nix` as usual.

Note
----
**GNAT, GPRBuild, and other Ada packages included in `nixpkgs` should be taken from `nix-ada` instead (or `nix-ada.pkgs`)**

This is due to a version mismatch between GNAT and GCC in standard nixpkgs (12 vs 13) and the resulting ABI incompatibilities.

If you wish to link an Ada library from nixpkgs with an Ada library from nix-ada, this means you must pull the nixpkgs library from `nix-ada.pkgs.<library name>` instead of importing from `<nixpkgs>`.
