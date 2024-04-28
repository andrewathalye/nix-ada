{ stdenv
, fetchzip
, gnat
, gprbuild
, glibc
, pkg-config
}:

stdenv.mkDerivation rec {
  pname = "vss";
  version = "24.2";
  
  src = fetchzip {
    url = "https://github.com/AdaCore/VSS/archive/refs/heads/${version}.zip";
    sha256 = "sha256-Tgu+0vlfgM6uZo5SwQk6nV67YCGI6VOOj32pHlOtjU0=";
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
