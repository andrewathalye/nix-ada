{ stdenv
, fetchgit
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

stdenv.mkDerivation rec {
  pname = "libadalang";
  version = "24.2";
  
  src = fetchGit {
    url = "https://github.com/AdaCore/libadalang.git";
    ref = version;
    rev = "78f3b1e72bb2d1c9cf25a5eaaa784ea9653e29b3";
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
