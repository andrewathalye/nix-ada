{ stdenv
, fetchgit
, fetchurl

# Build-time
, gnat
, gprbuild
, pkg-config
, wrapGAppsHook3

# Python
, python3
, python3Packages
, libadalang-python

# Deps
, gnatPackages
, gtkada
, libadalang
, libadalang-tools
, vss-extra
, ada-language-server
, ada-language-server-glib
, ada-spawn-glib
, hicolor-icon-theme
, gobject-introspection
, gdb
}:

with python3Packages;
with gnatPackages;
let
  ourPython = (python3.withPackages (ps: with ps; [ pycairo pygobject3 libadalang-python jedi pyyaml pycodestyle distutils ]));
in
stdenv.mkDerivation rec {
  pname = "gnatstudio";
  version = "20260109-git";
  
  src = fetchGit {
    url = "https://github.com/AdaCore/gnatstudio.git";
    ref = "master";
    rev = "1c2f3eab14871884fa532651345ae5546192ddd0";
  };

  gnatswitches_file = fetchurl {
    url = "https://raw.githubusercontent.com/gcc-mirror/gcc/298a486c58180adddd99c81217b394f7e4d4bd35/gcc/ada/doc/gnat_ugn/building_executable_programs_with_gnat.rst";
    sha256 = "LhJJz7wJdK+MOyoEhz1Ha60ETvOv1NBVnX3InzIDysY=";
  };

  patches = [
  ];

  nativeBuildInputs = [
    gprbuild
    gnatcoll-db2ada
    pkg-config
    ourPython
    wrapGAppsHook3
  ];

  buildInputs = [
    xmlada
    pygobject3
    pycairo
    gtkada
    gnatcoll-core
    gnatcoll-python3
    libadalang
    vss-extra
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
