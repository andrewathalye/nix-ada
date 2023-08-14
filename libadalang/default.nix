{ stdenv
, fetchzip
, gnat
, gprbuild
, glibc
, pkg-config
, gnatcoll-core
, gnatcoll-gmp
, gnatcoll-iconv
, langkit-support
, langkit
, libgpr2
, python3
}:

stdenv.mkDerivation {
  pname = "libadalang";
  version = "23.0.0-20230803-git";
  
  src = fetchzip {
    url = "https://github.com/AdaCore/libadalang/archive/db0eb8bae58de51d361ee638f23c855a4d7c1a9c.zip";
    sha256 = "D0+njgeJyvYfgbXbOERaqraVwsHAJftQSYeixXo+G90=";
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
    langkit-support
    libgpr2
  ];

  configurePhase = ''
    runHook preConfigure
    python3 manage.py generate
    runHook postConfigure
  '';

  buildPhase = ''
    runHook preBuild
    python manage.py build --disable-build-warnings --library-types=relocatable
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall
    python manage.py install $out --library-types=relocatable
    runHook postInstall
  '';
}
