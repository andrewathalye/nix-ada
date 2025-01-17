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
, gnatformat
, glibSupport ? false
}:

let
   inherit (lib) optionalString optional;
in
stdenv.mkDerivation rec {
  pname = "ada-language-server" + optionalString glibSupport "-glib";
  version = "26.0.20412191";
  
  src = fetchGit {
    url = "https://github.com/AdaCore/ada_language_server.git";
    ref = "master";
    rev = "cd011faf96e1a7b84148ef65a0eeeb14c25a7c54";
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
    gnatformat
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
