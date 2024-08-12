{ stdenv
, fetchgit
, gnat
, gprbuild
, pkg-config
}:

stdenv.mkDerivation rec {
  pname = "vss";
  version = "24.2-20240528";
  
  src = fetchGit {
    url = "https://github.com/AdaCore/VSS.git";
    ref = "master";
    rev = "75f32463d38a184a21af5a371fdde2f6dd2641c2";
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
