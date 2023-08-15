{ stdenv
, fetchzip
, gnat
, gprbuild
, glibc
, pkg-config
}:

stdenv.mkDerivation {
  pname = "vss";
  version = "23.0.0-20230725-git";
  
  src = fetchzip {
    url = "https://github.com/AdaCore/VSS/archive/719386b1d0a013b3ef9b16d7ce271310a7f12582.zip";
    sha256 = "EOkfABXaXv7Ml9a35nLnbRg8ZLPCHp/oQNP54pzynnc=";
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
