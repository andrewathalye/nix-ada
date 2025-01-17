{ stdenv
, fetchgit
, gnat
, gprbuild
}:

stdenv.mkDerivation {
  pname = "adasat";
  version = "25.0.0";
  
  src = fetchGit {
    url = "https://github.com/AdaCore/AdaSAT.git";
    ref = "master";
    rev = "01e9a19b61ba785878862b8bce5ae8145018ef01";
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

    gprinstall -a --prefix=$out -XLIBRARY_TYPE=relocatable -XBUILD_MODE=prod -p -Padasat

    runHook postInstall
  '';
}
