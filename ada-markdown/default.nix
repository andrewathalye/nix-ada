{ stdenv
, fetchzip
, gnat
, gprbuild
, glibc
, vss
}:

stdenv.mkDerivation rec {
  pname = "ada-markdown";
  version = "24.2";
  
  src = fetchzip {
    url = "https://github.com/AdaCore/markdown/archive/refs/heads/${version}.zip";
    sha256 = "sha256-d8dYok51bxITYdEQog1n2vfnybEFmzxcnf85nNVHyZA=";
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
