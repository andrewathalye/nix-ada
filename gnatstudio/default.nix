{ stdenv
, fetchgit
, fetchurl

# Build-time
, gnat
, gprbuild
, glibc
, pkg-config
, gnatcoll-db2ada
, wrapGAppsHook

# Python
, pygobject3
, pycairo
, cairo
, python3
, jedi
, pyyaml
, pycodestyle
, libadalang-python

# Deps
, xmlada
, gnatcoll-core
, gtkada
, gnatcoll-python3-patched
, libadalang
, libadalang-tools
, vss
, ada-language-server
, ada-language-server-glib
, ada-spawn-glib
, gnatcoll-xref
, gnatcoll-sqlite
, hicolor-icon-theme
, gobject-introspection
, gdb
}:

let
  ourPython = (python3.withPackages (ps: with ps; [ pycairo pygobject3 libadalang-python jedi pyyaml pycodestyle ]));
in
stdenv.mkDerivation rec {
  pname = "gnatstudio";
  version = "24.2";
  
  src = fetchGit {
    url = "https://github.com/AdaCore/gnatstudio.git";
    ref = version;
    rev = "ac65e0afa5d9d959fe3f9f958820e9c6614b2e8e";
  };

  gnatswitches_file = fetchurl {
    url = "https://raw.githubusercontent.com/gcc-mirror/gcc/298a486c58180adddd99c81217b394f7e4d4bd35/gcc/ada/doc/gnat_ugn/building_executable_programs_with_gnat.rst";
    sha256 = "LhJJz7wJdK+MOyoEhz1Ha60ETvOv1NBVnX3InzIDysY=";
  };

  patches = [
    ./dap-vss.patch
    ./dap-clients.patch
    ./codepeer-bridge-commands.patch
    ./parallel-cli.patch
    ./collections-abc.patch
  ];

  nativeBuildInputs = [
    gprbuild
    gnatcoll-db2ada
    pkg-config
    ourPython
    wrapGAppsHook
  ];

  buildInputs = [
    xmlada
    pygobject3
    pycairo
    gtkada
    gnatcoll-core
    gnatcoll-python3-patched
    libadalang
    vss
    ada-language-server-glib
    gnatcoll-sqlite
    gnatcoll-xref
    ada-spawn-glib
    hicolor-icon-theme
    gobject-introspection
    ourPython
  ];

  propagatedNativeBuildInputs = [
    ada-language-server
    libadalang-tools
    gdb
    gnat
    gprbuild
  ];

  buildPhase = ''
    runHook preBuild

    # Copy necessary GNAT sources (just the one atm)
    ln -s $gnatswitches_file gnat/building_executable_programs_with_gnat.rst

    LAL_TOOLS_BUILD=default BUILD=Production LIBRARY_TYPE=relocatable make

    runHook postBuild
  '';

  # Leave a reference to the Python env so that gnatstudio can find it
  # TODO hacky?
  ourPython_out = ourPython.out;
  postFixup = ''
    ln -s $ourPython_out $out/share/gnatstudio/python
  '';
}
