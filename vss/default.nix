{ stdenv
, fetchgit
, gnat
, gprbuild
, glibc
, pkg-config
}:

stdenv.mkDerivation rec {
  pname = "vss";
  version = "24.2";
  
  src = fetchGit {
    url = "https://github.com/AdaCore/VSS.git";
    ref = version;
    rev = "1c49a43be7e29b609b024aec438e65e0c46aff0d";
  };
  
  nativeBuildInputs = [
    gprbuild
    gnat
    pkg-config
  ];

  buildPhase = ''
    runHook preBuild
    make PREFIX=$out BUILD_MODE=prod
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall
    make PREFIX=$out BUILD_MODE=prod install
    runHook postInstall
  '';
}
