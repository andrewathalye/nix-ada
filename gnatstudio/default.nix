{ stdenv
, fetchzip
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
, python3
, jedi
, pyyaml
, pycodestyle
, libadalang-python

# Deps
, xmlada
, gnatcoll-core
, gtkada
, gnatcoll-python3
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
stdenv.mkDerivation {
  pname = "gnatstudio";
  version = "23.0.0-20230809-git";
  
  src = fetchzip {
    url = "https://github.com/AdaCore/gnatstudio/archive/8b1a2b2c5bb796830e27ba5b459c3e9ae93f428d.zip";
    sha256 = "6imb4XvjmCgAV6ZetdCJpZVbw8g5GbsA7lP9VY5sl8k=";
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
    gnat
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
    gnatcoll-python3
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

  propagatedBuildInputs = [
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
