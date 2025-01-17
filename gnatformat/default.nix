{ stdenv
, fetchgit
, gnat
, gprbuild
, libadalang
, prettier-ada
}:

stdenv.mkDerivation rec {
  pname = "gnatformat";
  version = "25.0.0-20241220";
  
  src = fetchGit {
    url = "https://github.com/AdaCore/gnatformat.git";
    ref = "main";
    rev = "f35ab68a012d392f939a4585194ce56d666b56bf";
  };
  
  nativeBuildInputs = [
    gprbuild
    gnat
  ];

  buildInputs = [
    libadalang
    prettier-ada
  ];

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
