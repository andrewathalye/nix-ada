{ stdenv
, fetchzip
, gnat
, gprbuild
, glibc
}:

stdenv.mkDerivation {
  pname = "adasat";
  version = "23.0.0-20230120-git";
  
  src = fetchzip {
    url = "https://github.com/AdaCore/AdaSAT/archive/f948e2271aec51f9313fa41ff3c00230a483f9e8.zip";
    sha256 = "3xSDxT1oSr7lTJm+ncI9cowtAC8hPxWY1LPZzk8V/nk=";
  };
  
  nativeBuildInputs = [
    gprbuild
    gnat
  ];

  dontConfigure = true;

  buildPhase = ''
    runHook preBuild
    make LIBRARY_TYPE=relocatable BUILD_MODE=prod lib
    runHook postBuild
  '';
  
  installPhase = ''
    runHook preInstall

    gprinstall --prefix=$out -XLIBRARY_TYPE=relocatable -XBUILD_MODE=prod -p -Padasat

    runHook postInstall
  '';
}
