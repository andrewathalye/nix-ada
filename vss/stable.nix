{ stdenv
, fetchzip
, gnat
, gprbuild
, glibc
, pkg-config
}:

stdenv.mkDerivation {
  pname = "vss";
  version = "23.0.0";
  
  src = fetchzip {
    url = "https://github.com/AdaCore/VSS/archive/refs/tags/v23.0.0.zip";
    sha256 = "0y+tgxTK8VuscEdwoVyXfKsPMJxSErWrtDKBQIFhRwg=";
  };
  
  nativeBuildInputs = [
    gprbuild
    gnat
    pkg-config
  ];

  buildPhase = ''
    runHook preBuild
    LIBRARY_TYPE=relocatable make BUILD_MODE=prod 
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall
    LIBRARY_TYPE=relocatable make PREFIX=$out BUILD_MODE=prod install
    runHook postInstall
  '';
}
