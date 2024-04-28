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

stdenv.mkDerivation rec {
  pname = "libadalang";
  version = "24.2";
  
  src = fetchzip {
    url = "https://github.com/AdaCore/libadalang/archive/refs/heads/${version}.zip";
    sha256 = "sha256-vmPavzFXGvZnxSJiMX8XZ82saDxVBrqlPoXKT0WXKLc=";
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
