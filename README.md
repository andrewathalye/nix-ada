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

Bear in mind the following:

**GNAT, GPRBuild, and other Ada packages included in `nixpkgs` should be taken from the attribute `adapkgs`**

This is due to a version mismatch between GNAT and GCC in standard nixpkgs (12 vs 13) and the resulting ABI incompatibilities.

Therefore, if you want to link both an Ada library from nixpkgs and an Ada library from nix-ada into your program, make sure to use `adapkgs` instead of the standard `<nixpkgs>`
