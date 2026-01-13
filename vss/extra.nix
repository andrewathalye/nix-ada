{ stdenv
, fetchgit
, gnat
, gprbuild
, pkg-config
, vss-text
}:

stdenv.mkDerivation rec {
  pname = "vss-extra";
  version = "26.0.0-20260109";
  
  src = fetchGit {
    url = "https://github.com/AdaCore/vss-extra.git";
    ref = "master";
    rev = "f52c33dcc503029af4dbe98f24b4fc5800570a7d";
  };
  
  nativeBuildInputs = [
    gprbuild
    gnat
    pkg-config
  ];

  propagatedBuildInputs = [
    vss-text
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
