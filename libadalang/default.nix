{ stdenv
, fetchgit
, gnat
, gprbuild
, pkg-config
, gnatcoll-core
, gnatcoll-gmp
, gnatcoll-iconv
, langkit-support
, langkit
, langkit-lktlang
, libgpr2
, python3
}:

stdenv.mkDerivation rec {
  pname = "libadalang";
  version = "25.0.0-20250115";
  
  src = fetchGit {
    url = "https://github.com/AdaCore/libadalang.git";
    ref = "master";
    rev = "020ddb23c2824083c6cb21b3d3594fb7db01c604";
  };

  nativeBuildInputs = [
    gprbuild
    gnat
    pkg-config
    python3
  ];

  propagatedBuildInputs = [
    gnatcoll-core
    gnatcoll-gmp
    gnatcoll-iconv
    langkit
    langkit-lktlang
    langkit-support
    libgpr2
  ];

  configurePhase = ''
    runHook preConfigure

    # TODO workaround for lktlang bug
    LD_LIBRARY_PATH=${langkit-lktlang.out}/lib python3 manage.py generate
    runHook postConfigure
  '';

  buildPhase = ''
    runHook preBuild
    python manage.py build --disable-build-warnings
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall
    python manage.py install $out
    runHook postInstall
  '';
}
