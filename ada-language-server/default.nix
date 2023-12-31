{ lib
, stdenv
, fetchzip
, gnat
, gprbuild
, glibc
, libadalang
, libadalang-python
, libadalang-tools
, vss
, libgnatdoc
, libgpr2
, ada-spawn
, ada-spawn-glib
, lal-refactor
, ada-libfswatch
, glibSupport ? false
}:

let
   inherit (lib) optionalString optional;
in
stdenv.mkDerivation {
  pname = "ada-language-server" + optionalString glibSupport "-glib";
  version = "23.0.21-20230814-git";
  
  src = fetchzip {
    url = "https://github.com/AdaCore/ada_language_server/archive/03266f89ddc540c332e02a56402af080aad0a3ec.zip";
    sha256 = "rjvhy5u/ygHe0b+TQSRRx8hpV/IpZhZ93JPkMPLI2rQ=";
  };

  nativeBuildInputs = [
    gprbuild
    gnat
  ];

  buildInputs = [
    libadalang
    libadalang-tools
    vss
    libgnatdoc
    libgpr2
    lal-refactor
    ada-libfswatch
  ]
  ++ optional glibSupport ada-spawn-glib
  ++ optional (!glibSupport) ada-spawn;

  dontConfigure = true;

  COMMON_OPTS="-XLAL_TOOLS_BUILD=default -XLIBRARY_TYPE=relocatable -XBUILD_MODE=prod";

  buildPhase = ''
    runHook preBuild

    export BUILD_OPTS="$COMMON_OPTS -j0"
    ''
    + optionalString (!glibSupport)
    '' # Build custom System.Memory version
      gprbuild $BUILD_OPTS -d -ws -c -u -P gnat/lsp_server.gpr s-memory.adb
      gprbuild $BUILD_OPTS -Pgnat/lsp.gpr
      gprbuild $BUILD_OPTS -Pgnat/lsp_server.gpr
    ''
    + optionalString glibSupport "gprbuild $BUILD_OPTS -Pgnat/lsp_client_glib.gpr"
    + ''

    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall

    export INSTALL_OPTS="$COMMON_OPTS -p --prefix=$out --no-manifest -m"
    ''
    + optionalString (!glibSupport) "gprinstall $INSTALL_OPTS --mode=usage -Pgnat/lsp_server.gpr "
    + optionalString glibSupport "gprinstall $INSTALL_OPTS -r -Pgnat/lsp_client_glib.gpr"
    + ''

    runHook postInstall
  '';
}
