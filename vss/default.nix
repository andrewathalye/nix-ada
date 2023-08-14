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

  installPhase = ''
    runHook preInstall
    make install PREFIX=$out
    runHook postInstall
  '';
}
