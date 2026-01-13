{ stdenv
, fetchgit
, gnat
, gprbuild
}:

stdenv.mkDerivation rec {
  pname = "xdiff";
  version = "26.1-20250930";
  
  src = fetchGit {
    url = "https://github.com/AdaCore/xdiff.git";
    ref = "main";
    rev = "1911fe55bc5f8b6f28e0968d0f6b9eca35484160";
  };
  
  nativeBuildInputs = [
    gprbuild
    gnat
  ];

  dontConfigure = true;
  
  installPhase = ''
    runHook preInstall

    make DESTDIR=$out install

    runHook postInstall
  '';
}
