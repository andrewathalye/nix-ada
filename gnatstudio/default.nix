{ stdenv
, fetchzip
, gnat
, gprbuild
, glibc
, pkg-config
, gtkada
, xmlada
, pygobject3
, pycairo
, python3
, gnatcoll-db2ada
, gnatcoll-core
, gnatcoll-python3
, libadalang
, libadalang-tools
, vss
, ada-language-server
, ada-language-server-glib
, ada-spawn-glib
, gnatcoll-xref
, gnatcoll-sqlite
}:

stdenv.mkDerivation {
  pname = "gnatstudio";
  version = "23.0.0-20230809-git";
  
  src = fetchzip {
    url = "https://github.com/AdaCore/gnatstudio/archive/8b1a2b2c5bb796830e27ba5b459c3e9ae93f428d.zip";
    sha256 = "6imb4XvjmCgAV6ZetdCJpZVbw8g5GbsA7lP9VY5sl8k=";
  };

  patches = [ ./dap-vss.patch ];
  
  nativeBuildInputs = [
    gprbuild
    gnat
    (python3.withPackages (ps: with ps; [ pycairo pygobject3 ]))
    gnatcoll-db2ada
    pkg-config
  ];

  buildInputs = [
    xmlada
    pygobject3
    pycairo
    gtkada
    gnatcoll-core
    gnatcoll-python3
    libadalang
    libadalang-tools
    vss
    ada-language-server
    ada-language-server-glib
    gnatcoll-sqlite
    gnatcoll-xref
    ada-spawn-glib
  ];

  dontConfigure = true;

  buildPhase = ''
    runHook preBuild

    LAL_TOOLS_BUILD=default BUILD=Production LIBRARY_TYPE=relocatable make

    runHook postBuild
  '';
}
