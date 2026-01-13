{ stdenv
, fetchgit
, gnat
, gprbuild
, ada-toml
, gnatPackages
}:

with gnatPackages;
stdenv.mkDerivation rec {
  pname = "stable-sloc";
  version = "unstable";
  
  src = ./src;
  
  nativeBuildInputs = [
    gprbuild
    gnat
  ];

  propagatedBuildInputs = [
    gnatcoll-core
    ada-toml
  ];

  dontConfigure = true;
  
  buildPhase = ''
    runHook preBuild
    
    gprbuild -j0 -Pstable_sloc -XLIBRARY_TYPE=relocatable
    
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall

    mkdir -p $out/share
    gprinstall --prefix=$out -p -Pstable_sloc -XLIBRARY_TYPE=relocatable

    runHook postInstall
  '';
}
