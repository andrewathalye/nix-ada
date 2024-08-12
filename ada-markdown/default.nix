{ stdenv
, fetchgit
, gnat
, gprbuild
, vss
}:

stdenv.mkDerivation rec {
  pname = "ada-markdown";
  version = "24.2";
  
  src = fetchGit {
    url = "https://github.com/AdaCore/markdown.git";
    ref = version;
    rev = "522ac0682d656f7a8c4a1d0116a8ddd732d2dad9";
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
