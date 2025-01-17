{ stdenv
, fetchgit
, gnat
, gprbuild
, pkg-config
}:

stdenv.mkDerivation rec {
  pname = "vss";
  version = "25.0.0";
  
  src = fetchGit {
    url = "https://github.com/AdaCore/VSS.git";
    ref = "master";
    rev = "3a9fb026953a47175605ae8d005cc7e7c3dc07c8";
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
