{ stdenv
, fetchzip
, gnat
, gprbuild
, gnatcoll-core
, gnatcoll-iconv
, gnatcoll-gmp
, langkit
, python3
, adasat
}:

stdenv.mkDerivation {
  pname = "langkit-support";
  version = langkit.version;
  
  src = langkit.src;

  nativeBuildInputs = [
    gprbuild
    gnat
    langkit
    python3
  ];

  buildInputs = [
    langkit
    gnatcoll-core
    gnatcoll-iconv
    gnatcoll-gmp
  ];

  propagatedBuildInputs = [
    adasat
  ];

  dontConfigure = true;
  
  buildPhase = ''
    runHook preBuild

    python3 manage.py build-langkit-support
    
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall

    python3 manage.py install-langkit-support $out

    runHook postInstall
  '';
}
