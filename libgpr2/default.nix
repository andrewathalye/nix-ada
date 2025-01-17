{ stdenv
, fetchgit
, gnat
, gprbuild
, langkit-support
, gnatcoll-core
, gnatcoll-iconv
, gnatcoll-gmp
, which
}:

stdenv.mkDerivation rec {
  pname = "libgpr2";
  version = "25.0.0-20250116";
  
  src = fetchGit {
    url = "https://github.com/AdaCore/gpr.git";
    ref = "main";
    rev = "1b614e9fbd9ecf6dfaee8573b6a68414bce3fc41";
  };

  gprconfig_kb_src = fetchGit {
    url = "https://github.com/AdaCore/gprconfig_kb.git";
    ref = "master";
    rev = "b732437d7828ae83fbdc549bd5e145703e8282cd";
  };
  
  nativeBuildInputs = [
    gprbuild
    gnat
    which
  ];

  buildInputs = [
    langkit-support
    gnatcoll-core
    gnatcoll-iconv
    gnatcoll-gmp
  ];

  configurePhase = ''
    runHook preConfigure
    make GPR2KBDIR=$gprconfig_kb_src/db/ prefix=$out ENABLED_SHARED=true setup
    runHook postConfigure
  '';

  buildPhase = ''
    runHook preBuild
    
    make build-lib-relocatable

    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall

    make install-lib-relocatable

    runHook postInstall
  '';
}
