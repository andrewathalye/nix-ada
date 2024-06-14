{ system ? builtins.currentSystem
, adapkgs-internal ? import <nixpkgs> {inherit system; overlays =
   [
    (final: prev: { gnat = prev.gnat13; })
    (final: prev: { gnat12 = prev.gnat13; })
   ];}
}:

# We require GNAT 13 to match the current system compiler GCC 13
with adapkgs-internal;
with python3Packages;
rec {

   adapkgs = adapkgs-internal;
   
   # Tier A
   gtkada = callPackage ./gtkada {};
   vss-stable = callPackage ./vss/stable.nix {};
   aws = callPackage ./aws {};
   vss = callPackage ./vss {};
   ada-libfswatch = callPackage ./ada-libfswatch {};
   templates-parser = callPackage ./templates-parser {};

   # Updated GNATCOLL for Python <=3.12 support
   gnatcoll-python3-p1 = gnatcoll-python3.overrideAttrs (final:
    {
      version = "24.2";
      src = fetchzip {
         url = "https://github.com/AdaCore/gnatcoll-bindings/archive/refs/heads/24.2.zip";
         hash = "sha256-LoeZspeO5siSuIcA6iuRABlpfSlpJJuxnhSvlbIYfzE=";
      };
    });
   gnatcoll-python3-patched = gnatcoll-python3-p1.override {python3 = python3;};
   # Force this gnatcoll to use the current default Python version (instead of hardcoded 3.9)

   types-gdb = callPackage ./types-gdb {};  
   e3-core = callPackage ./e3-core {};
   e3-testsuite = callPackage ./e3-testsuite {e3-core = e3-core;};
   gnat-gdb-scripts = callPackage ./gnat-gdb-scripts {};

   adasat = callPackage ./adasat {};

   # Tier B
   langkit = callPackage ./langkit {adasat = adasat;  types-gdb = types-gdb; e3-core = e3-core; e3-testsuite = e3-testsuite; gnat-gdb-scripts = gnat-gdb-scripts;};
   langkit-support = callPackage ./langkit-support {langkit = langkit;};
   libgpr2 = callPackage ./libgpr2 {langkit-support = langkit-support;}; # not quite Group A
   libadalang = callPackage ./libadalang {langkit-support = langkit-support; langkit = langkit; libgpr2 = libgpr2;};
   libadalang-python = callPackage ./libadalang/python.nix {libadalang = libadalang;};

   ada-spawn = callPackage ./ada-spawn {gtkada = gtkada;};
   ada-spawn-glib = ada-spawn.override { glibSupport = true; };

   ada-markdown = callPackage ./ada-markdown {vss = vss;};
   libgnatdoc = callPackage ./gnatdoc {libadalang = libadalang; vss = vss; ada-markdown = ada-markdown;};

   # Tier C
   libadalang-tools = callPackage ./libadalang-tools {libadalang = libadalang; templates-parser=templates-parser; vss = vss;};
   lal-refactor = callPackage ./lal-refactor {libadalang-tools = libadalang-tools; vss = vss;};
   
   # Tier D
   ada-language-server = callPackage ./ada-language-server {libadalang = libadalang; libadalang-tools = libadalang-tools; vss = vss; ada-spawn = ada-spawn; ada-spawn-glib = ada-spawn-glib; libgnatdoc = libgnatdoc; libgpr2 = libgpr2; lal-refactor = lal-refactor; ada-libfswatch = ada-libfswatch; libadalang-python = libadalang-python;};
   ada-language-server-glib = ada-language-server.override { glibSupport = true; };

   # Tier E
   gnatstudio = callPackage ./gnatstudio { gtkada = gtkada; libadalang = libadalang; libadalang-python = libadalang-python; libadalang-tools = libadalang-tools; vss = vss; ada-spawn-glib = ada-spawn-glib; ada-language-server-glib = ada-language-server-glib; ada-language-server = ada-language-server; gnatcoll-python3-patched = gnatcoll-python3-patched;};
}
