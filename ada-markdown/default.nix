{ stdenv
, fetchzip
, gnat
, gprbuild
, glibc
, vss
}:

stdenv.mkDerivation {
  pname = "ada-markdown";
  version = "23.0.0-20230514-git";
  
  src = fetchzip {
    url = "https://github.com/AdaCore/markdown/archive/522ac0682d656f7a8c4a1d0116a8ddd732d2dad9.zip";
    sha256 = "d8dYok51bxITYdEQog1n2vfnybEFmzxcnf85nNVHyZA=";
  };
  
  nativeBuildInputs = [
    gprbuild
    gnat
  ];

  buildInputs = [
    vss
  ];

  dontConfigure = true;
  
  buildPhase = ''
    runHook preBuild
    
    gprbuild -j0 -Pgnat/markdown.gpr
    
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall

    gprinstall --prefix=$out -p -Pgnat/markdown.gpr

    runHook postInstall
  '';
}
