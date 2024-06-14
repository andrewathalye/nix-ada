Nix-Ada
=======

Highly experimental set of Nix packages for Ada libraries and applications.
No guarantees yet :(

Of note:
libadalang
libadalang-tools
ada-language-server
gnatstudio



IMPORTANT MESSAGE
=================
gnat must be the same version as gcc / g++ on your system.
If that isn’t the case for any reason, override gnat or gcc / g++.

Otherwise libfswatch and ada-libfswatch will both try to load different versions
of libstdc++.
