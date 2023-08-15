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

  installsh = ./install.sh;
  
  nativeBuildInputs = [
    gprbuild
    gnat
    pkg-config
  ];

  # Manual support for multiple build types since
  # 23.0.0 did not support this on release
  buildPhase = ''
    runHook preBuild
    VSS_LIBRARY_TYPE=static make BUILD_MODE=prod
    VSS_LIBRARY_TYPE=static-pic XMLADA_BUILD=static-pic make BUILD_MODE=prod
    VSS_LIBRARY_TYPE=relocatable XMLADA_BUILD=relocatable make BUILD_MODE=prod
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall
    . $installsh
    installFunc static
    installFunc static-pic
    installFunc relocatable
    runHook postInstall
  '';
}
