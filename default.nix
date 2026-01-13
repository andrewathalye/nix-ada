{ pkgs }:

with pkgs;
rec {
   # Any other Ada packages can be found in pkgs.X
   inherit pkgs;
   
   # Tier A
   gtkada = callPackage ./gtkada {};
   vss-stable = callPackage ./vss/stable.nix {};
   vss-text = callPackage ./vss {};
   vss-extra = callPackage ./vss/extra.nix  { inherit vss-text; };
   aws = callPackage ./aws {};
   ada-libfswatch = callPackage ./ada-libfswatch {};
   templates-parser = callPackage ./templates-parser {};
   florist = callPackage ./florist {};
   polyorb = callPackage ./polyorb {};
   gnat_util = callPackage ./gnat_util {};
   xdiff = callPackage ./xdiff {};
   ada-toml = callPackage ./ada-toml {};

   # Nixified version of Alire Index
   alire-index = callPackage ./alire-index {};

   types-gdb = callPackage ./types-gdb {};  
   e3-core = callPackage ./e3-core {};
   e3-testsuite = callPackage ./e3-testsuite { inherit e3-core; };
   gnat-gdb-scripts = callPackage ./gnat-gdb-scripts {};
   adasat = callPackage ./adasat {};

   # Tier B
   prettier-ada = callPackage ./prettier-ada { inherit vss-text; };
   langkit = callPackage ./langkit { inherit types-gdb e3-core e3-testsuite gnat-gdb-scripts prettier-ada; };
   langkit-support = callPackage ./langkit/support.nix { inherit langkit adasat; };
   langkit-lktlang = callPackage ./langkit/lktlang.nix { inherit langkit langkit-support adasat; };
   libadalang = callPackage ./libadalang { inherit langkit langkit-lktlang langkit-support; };
   libadalang-python = callPackage ./libadalang/python.nix { inherit libadalang; };
   stable-sloc = callPackage ./stable-sloc { inherit ada-toml; };

   ada-spawn = callPackage ./ada-spawn { inherit gtkada; };
   ada-spawn-glib = ada-spawn.override { glibSupport = true; };

   ada-markdown = callPackage ./ada-markdown { inherit vss-extra; };
   libgnatdoc = callPackage ./gnatdoc/libgnatdoc.nix { inherit libadalang vss-extra ada-markdown; };

   wayland-ada-scanner = callPackage ./wayland-ada/scanner.nix { inherit alire-index; };
   wayland-ada = callPackage ./wayland-ada { inherit wayland-ada-scanner alire-index; };

   # Tier C
   gnatcoverage = callPackage ./gnatcoverage { inherit gnat_util libadalang templates-parser stable-sloc; };
   gnatformat = callPackage ./gnatformat { inherit prettier-ada libadalang vss-extra; };
   libadalang-tools = callPackage ./libadalang-tools { inherit libadalang templates-parser vss-text; };
   lal-refactor = callPackage ./lal-refactor { inherit libadalang-tools vss-extra; };
   gnatdoc = callPackage ./gnatdoc { inherit libgnatdoc vss-extra ada-markdown; };
   
   # Tier D
   ada-language-server = callPackage ./ada-language-server { inherit libadalang libadalang-tools vss-text ada-spawn ada-spawn-glib libgnatdoc lal-refactor ada-libfswatch libadalang-python gnatformat xdiff; };
   ada-language-server-glib = ada-language-server.override { glibSupport = true; };

   # Tier E
   gnatstudio = callPackage ./gnatstudio { inherit gtkada libadalang libadalang-python libadalang-tools vss-extra ada-spawn-glib ada-language-server-glib ada-language-server; };
}
