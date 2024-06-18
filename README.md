Nix-Ada
=======

A set of Nix packages for Ada libraries and applications. The set changes over time, so see `./default.nix` for a full, canonical list.

Any package in that list is certified to build and work on x86\_64-linux with the provided Flake.

Please create a Pull Request if you’d like to add a package or update one, or an Issue if something is wrong with an existing package.

Flake Info
----------
This repository provides a `flake.nix` file, which you may use if you so desire.

It is not necessary, however, and you can also import `default.nix` as usual.

Note
----
**GNAT, GPRBuild, and other Ada packages included in `nixpkgs` should be taken from `nix-ada` instead (or `nix-ada.pkgs`)**

This is due to a version mismatch between GNAT and GCC in standard nixpkgs (12 vs 13) and the resulting ABI incompatibilities.

If you wish to link an Ada library from nixpkgs with an Ada library from nix-ada, this means you must pull the nixpkgs library from `nix-ada.pkgs.<library name>` instead of importing from `<nixpkgs>`.
