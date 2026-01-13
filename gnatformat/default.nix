{ stdenv
, fetchgit
, gnat
, gprbuild
, libadalang
, prettier-ada
, vss-extra
}:

stdenv.mkDerivation rec {
  pname = "gnatformat";
  version = "26.0.0-20250924";
  
  src = fetchGit {
    url = "https://github.com/AdaCore/gnatformat.git";
    ref = "v26.0.0";
    rev = "976b276ba6910b535c740b392ca4f2df877a6c75";
  };
  
  nativeBuildInputs = [
    gprbuild
    gnat
  ];

  buildInputs = [
    libadalang
    prettier-ada
    vss-extra
  ];

  patchPhase = ''
    sed -i "s/ALL_LIBRARY_TYPES =.*/ALL_LIBRARY_TYPES = relocatable/" Makefile
  '';

  dontConfigure = true;
  
  buildPhase = ''
    runHook preBuild
    
    make lib bin BUILD_MODE=prod LIBRARY_TYPE=relocatable
    
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall

    make install BUILD_MODE=prod LIBRARY_TYPE=relocatable PREFIX=$out

    runHook postInstall
  '';
}
