{ pkgs
, dbus-ada }:

with pkgs;
with python3Packages;
rec {
   inherit gnat;
   inherit gprbuild;
   inherit alire;
   inherit dbus-ada;

   # Any other Ada packages can be found in pkgs.X
   # The above are inherited for convenience
   inherit pkgs;
   
   # Tier A
   gtkada = callPackage ./gtkada {};
   vss-stable = callPackage ./vss/stable.nix {};
   aws = callPackage ./aws {};
   vss = callPackage ./vss {};
   ada-libfswatch = callPackage ./ada-libfswatch {};
   templates-parser = callPackage ./templates-parser {};
   florist = callPackage ./florist {};
   polyorb = callPackage ./polyorb {};
   gnat_util = callPackage ./gnat_util {};
  
   # Nixified version of Alire Index
   alire-index = callPackage ./alire-index {};

   # Updated GNATCOLL for Python <=3.12 support
#   gnatcoll-python3-patched-p1 = gnatcoll-python3.overrideAttrs (final:
#    {
#      version = "24.2";
#      src = fetchzip {
#         url = "https://github.com/AdaCore/gnatcoll-bindings/archive/refs/heads/24.2.zip";
#         hash = "sha256-LoeZspeO5siSuIcA6iuRABlpfSlpJJuxnhSvlbIYfzE=";
#      };
#    });
#   gnatcoll-python3-patched = gnatcoll-python3-patched-p1.override { python3 = python3; };
   # Force this gnatcoll to use the current default Python version (instead of hardcoded 3.9)

   types-gdb = callPackage ./types-gdb {};  
   e3-core = callPackage ./e3-core {};
   e3-testsuite = callPackage ./e3-testsuite { inherit e3-core; };
   gnat-gdb-scripts = callPackage ./gnat-gdb-scripts {};

   adasat = callPackage ./adasat {};

   # Tier B
   prettier-ada = callPackage ./prettier-ada { inherit vss; };
   langkit = callPackage ./langkit { inherit adasat types-gdb e3-core e3-testsuite gnat-gdb-scripts prettier-ada; };
   langkit-support = callPackage ./langkit-support { inherit langkit; };
   libgpr2 = callPackage ./libgpr2 { inherit langkit-support; };
   libadalang = callPackage ./libadalang { inherit langkit langkit-support libgpr2; };
   libadalang-python = callPackage ./libadalang/python.nix { inherit libadalang; };

   ada-spawn = callPackage ./ada-spawn { inherit gtkada; };
   ada-spawn-glib = ada-spawn.override { glibSupport = true; };

   ada-markdown = callPackage ./ada-markdown { inherit vss; };
   libgnatdoc = callPackage ./gnatdoc/libgnatdoc.nix { inherit libadalang vss ada-markdown; };

   wayland-ada-scanner = callPackage ./wayland-ada/scanner.nix { inherit alire-index; };
   wayland-ada = callPackage ./wayland-ada { inherit wayland-ada-scanner alire-index; };

   # Tier C
   gnatcoverage = callPackage ./gnatcoverage { inherit gnat_util libadalang; };
   libadalang-tools = callPackage ./libadalang-tools { inherit libadalang templates-parser vss; };
   lal-refactor = callPackage ./lal-refactor { inherit libadalang-tools vss; };
   gnatdoc = callPackage ./gnatdoc { inherit libgnatdoc vss ada-markdown; };
   
   # Tier D
   ada-language-server = callPackage ./ada-language-server { inherit libadalang libadalang-tools vss ada-spawn ada-spawn-glib libgnatdoc libgpr2 lal-refactor ada-libfswatch libadalang-python; };
   ada-language-server-glib = ada-language-server.override { glibSupport = true; };

   # Tier E
   gnatstudio = callPackage ./gnatstudio { inherit gtkada libadalang libadalang-python libadalang-tools vss ada-spawn-glib ada-language-server-glib ada-language-server gnatcoll-python3; };
}
