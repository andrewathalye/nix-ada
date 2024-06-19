{ stdenv
, fetchgit
, gnat
, gprbuild
, glibc
, langkit-support
, gnatcoll-core
, gnatcoll-iconv
, gnatcoll-gmp
, which
}:

stdenv.mkDerivation rec {
  pname = "libgpr2";
  version = "24.2-20240611-git";
  
  src = fetchGit {
    url = "https://github.com/AdaCore/gpr.git";
    ref = "master";
    rev = "d9220e60b0b6b94785de22b173e73786b5872fd8";
  };

  gprconfig_kb_src = fetchGit {
    url = "https://github.com/AdaCore/gprconfig_kb.git";
    ref = version;
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
