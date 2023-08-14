{ stdenv
, fetchzip
, gnat
, gprbuild
, glibc
, gnatcoll-core
, gnatcoll-iconv
, gnatcoll-gmp
, langkit
, python3
}:

stdenv.mkDerivation {
  pname = "langkit-support";
  version = langkit.version;
  
  src = langkit.src;
  
  nativeBuildInputs = [
    gprbuild
    gnat
    langkit
  ];

  buildInputs = [
    langkit
    gnatcoll-core
    gnatcoll-iconv
    gnatcoll-gmp
  ];

  dontConfigure = true;
  
  buildPhase = ''
    runHook preBuild
    
    python3 manage.py build-langkit-support --library-types=relocatable
    
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall

    python3 manage.py install-langkit-support --library-types=relocatable $out

    runHook postInstall
  '';
}
