{ lib
, buildPythonPackage
, fetchgit
, gnat
, gprbuild
, gnatcoll-core
, gnatcoll-iconv
, gnatcoll-gmp

, langkit
, langkit-support
, adasat
}:

buildPythonPackage rec {
  pname = "langkit-lktlang";
  version = langkit.version;
  
  src = langkit.src;

  nativeBuildInputs = [
    gnat
    gprbuild
  ];

  buildInputs = [
    adasat
    gnatcoll-core
    gnatcoll-iconv
    gnatcoll-gmp
    langkit-support
  ];

  propagatedBuildInputs = [
    langkit
  ];

  configurePhase = ''
    runHook preConfigure

    # Copy AdaSAT sources
    ln -s ${adasat.src}/adasat.gpr langkit/support/adasat.gpr
    ln -s ${adasat.src}/src langkit/support/src

    python3 manage.py make --no-mypy
    cd contrib/lkt
    python3 manage.py install $out

    cd build/python

    runHook postConfigure
  '';
  doCheck = false;
}
