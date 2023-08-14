{ lib
, stdenv
, fetchzip
, gnat
, gprbuild
, glibc
, pkg-config
, libadalang
, libadalang-tools
, vss
, libgnatdoc
, libgpr2
, ada-spawn
, ada-spawn-glib
, templates-parser
, lal-refactor
, ada-libfswatch
, glibSupport ? false
}:

let
   inherit (lib) optionalString optional;
in
stdenv.mkDerivation {
  pname = "ada-language-server" + optionalString glibSupport "-glib";
  version = "23.0.21";
  
  src = fetchzip {
    url = "https://github.com/AdaCore/ada_language_server/archive/refs/tags/23.0.21.zip";
    sha256 = "WIG8B5LMdT2tpmValBUG2Tj9l2UHxyBozAszAzeKG58=";
  };
  
  nativeBuildInputs = [
    gprbuild
    gnat
    libadalang-tools # Must be executable on host
    pkg-config
  ];

  buildInputs = [
    libadalang
    libadalang-tools
    vss
    templates-parser
    libgnatdoc
    libgpr2
    lal-refactor
    ada-libfswatch
  ]
  ++ optional glibSupport ada-spawn-glib
  ++ optional (!glibSupport) ada-spawn;

  dontConfigure = true;

  buildPhase = ''
    runHook preBuild
    BUILD_MODE=prod LAL_TOOLS_BUILD=default ''
    + optionalString (!glibSupport) "make prefix=$out"
    + optionalString glibSupport "gprbuild -j0 -Pgnat/lsp_client_glib.gpr" + ''

    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall

    LAL_TOOLS_BUILD=default prefix=$out BUILD_MODE=prod ''
    + optionalString (!glibSupport) "make install"
    + optionalString glibSupport "gprinstall --prefix=$out -p -Pgnat/lsp_client_glib.gpr" + ''

    runHook postInstall
  '';
}
