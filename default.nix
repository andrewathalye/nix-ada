{ system ? builtins.currentSystem }:

with import <nixpkgs> {inherit system;};
with python310Packages;
rec {
   # Base set
   alire = callPackage ./alire {};
   gtkada = callPackage ./gtkada {};
   vss = callPackage ./vss {};
   ada-libfswatch = callPackage ./ada-libfswatch {};
   templates-parser = callPackage ./templates-parser {};

   # Overridden packages (force update to unstable)
   gnatcoll-gmp-updated = gnatcoll-gmp.overrideAttrs {
     version = "23.0.0-20230104";
     src = fetchzip {
       url = "https://github.com/AdaCore/gnatcoll-bindings/archive/411d4c6201dd4e2ca47e06ddc81c7cc9533c92f3.zip";
       sha256 = "VeIQ5dDaBla8L8NMt6WsQoU7EFfzygEDFScfk+udSgE=";
     };
   };

   # Group A dependencies
   types-gdb = callPackage ./types-gdb {};  
   e3-core = callPackage ./e3-core {};
   e3-testsuite = callPackage ./e3-testsuite {e3-core = e3-core;};
   gnat-gdb-scripts = callPackage ./gnat-gdb-scripts {};

   # Group A (must be fetched from same branch)
   adasat = callPackage ./adasat {};
   langkit = callPackage ./langkit {adasat = adasat;  types-gdb = types-gdb; e3-core = e3-core; e3-testsuite = e3-testsuite; gnat-gdb-scripts = gnat-gdb-scripts;};
   langkit-support = callPackage ./langkit-support {langkit = langkit;};
   libgpr2 = callPackage ./libgpr2 {langkit-support = langkit-support;}; # not quite Group A
   libadalang = callPackage ./libadalang {langkit-support = langkit-support; langkit = langkit; libgpr2 = libgpr2; gnatcoll-gmp = gnatcoll-gmp-updated;};

   libadalang-tools = callPackage ./libadalang-tools {libadalang = libadalang; templates-parser=templates-parser; vss = vss;};
   lal-refactor = callPackage ./lal-refactor {libadalang-tools = libadalang-tools; vss = vss;};

   # 100% independent (glib + gtkada)
   ada-spawn = callPackage ./ada-spawn {gtkada = gtkada;};
   ada-spawn-glib = ada-spawn.override { glibSupport = true; };

   # Mostly independent (uses vss + libadalang)
   ada-markdown = callPackage ./ada-markdown {vss = vss;};
   libgnatdoc = callPackage ./gnatdoc {libadalang = libadalang; vss = vss; ada-markdown = ada-markdown;};

   # Highly dependent (pick similar branches)
   ada-language-server = callPackage ./ada-language-server {libadalang = libadalang; libadalang-tools = libadalang-tools; vss = vss; templates-parser = templates-parser; ada-spawn = ada-spawn; ada-spawn-glib = ada-spawn-glib; libgnatdoc = libgnatdoc; libgpr2 = libgpr2; lal-refactor = lal-refactor; ada-libfswatch = ada-libfswatch;};
   ada-language-server-glib = ada-language-server.override { glibSupport = true; };

   # The big one
   gnatstudio = callPackage ./gnatstudio { gtkada = gtkada; libadalang = libadalang; libadalang-tools = libadalang-tools; vss = vss; ada-language-server = ada-language-server; ada-spawn-glib = ada-spawn-glib; ada-language-server-glib = ada-language-server-glib;};
}
