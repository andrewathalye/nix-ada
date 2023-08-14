{ stdenv
, fetchzip
, gnat
, gprbuild
, glibc
, langkit-support
, gnatcoll-iconv
, gnatcoll-gmp
, which
}:

stdenv.mkDerivation {
  pname = "libgpr2";
  version = "23.0.0-20230811-git";
  
  src = fetchzip {
    url = "https://github.com/AdaCore/gpr/archive/283892f08eee98bc9ba9adfa60200b7bef9d4bbb.zip";
    sha256 = "PlOHZESIJGJnE89A4MVbD6Z7XFMvy69q06DU3+ofsMY=";
  };

  # 20230811-git
  gprconfig_kb_src = fetchzip {
    url = "https://github.com/AdaCore/gprconfig_kb/archive/9dc09852662069ae34511e0950ea9efc5bb4eb4d.tar.gz";
    sha256 = "IP/TMxyhjWfIU5Z4tKbvaeFHMP/ToYPZrCO56q6dqWM=";
  };
  
  nativeBuildInputs = [
    gprbuild
    gnat
    which
  ];

  buildInputs = [
    langkit-support
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
