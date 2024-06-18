{ stdenv
, fetchgit
, gnat
, gprbuild
, glibc
, pkg-config
, which
}:

stdenv.mkDerivation rec {
  pname = "templates-parser";
  version = "24.2";
  
  src = fetchGit {
    url = "https://github.com/AdaCore/templates-parser.git";
    ref = version;
    rev = "53cff2e1a864f9da270bc610765283dd800f671c";
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
