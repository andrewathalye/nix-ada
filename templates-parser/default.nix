{ stdenv
, fetchzip
, gnat
, gprbuild
, glibc
, pkg-config
, which
}:

stdenv.mkDerivation {
  pname = "templates-parser";
  version = "23.0.0";
  
  src = fetchzip {
    url = "https://github.com/AdaCore/templates-parser/archive/refs/tags/v23.0.0.zip";
    sha256 = "vj1I8arBP/kjWVWC4hlyfo1eyBOF78F+gCCzb7lri+w=";
  };
  
  nativeBuildInputs = [
    gprbuild
    gnat
    which
    pkg-config
  ];

  buildPhase = ''
    runHook preBuild

    make prefix=$out PROCESSORS=0 ENABLE_SHARED=true

    runHook postBuild
  '';
}
