{ lib
, fetchgit
, gnat
, gprbuild
, gnatPackages

, python3Packages
, langkit
, langkit-support
, adasat
}:

with python3Packages;
with gnatPackages;
buildPythonPackage rec {
  pname = "langkit-lktlang";
  version = langkit.version;

  pyproject = true;
  build-system = [ setuptools ];
  
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
    python3 -m langkit.scripts.lkm install -c lkt/langkit.yaml $out --disable-all-mains
    # Change to directory so setuptools can run
    cd lkt/build/python

    runHook postConfigure
  '';

  doCheck = false;
}
