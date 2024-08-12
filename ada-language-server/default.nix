{ lib
, stdenv
, fetchgit
, gnat
, gprbuild
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
stdenv.mkDerivation rec {
  pname = "ada-language-server" + optionalString glibSupport "-glib";
  version = "24.2-20240618-git";
  
  src = fetchGit {
    url = "https://github.com/AdaCore/ada_language_server.git";
    ref = "master";
    rev = "cdcb01019e441f0e117376c84aa4aebc315d5173";
  };

  # Patch for strange libgpr2 quirk
  # 20240618
  patches = [ ./lsp-gpr_files.adb.patch ];

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
