{ stdenv
, gnat
, fetchzip
, gprbuild
, gnatPackages
, libadalang
, zlib
, libbfd
, libopcodes
, libiberty
, binutils
, gnat_util
, templates-parser
, stable-sloc
}:

with gnatPackages;
stdenv.mkDerivation rec {
  pname = "gnatcoverage";
  version = "26.0.0-20250112";
  
  src = fetchGit {
    url = "https://github.com/AdaCore/gnatcoverage.git";
    ref = "master";
    rev = "179943110f2bcb7ddfe7fc5bd11ddc00573825cc";
  };

  patches = [ ./gnatcov.gpr.patch ];
  
  nativeBuildInputs = [
    gprbuild
    gnat
  ];

  buildInputs = [
    gnatcoll-core
    libadalang
    zlib
    libbfd
    libopcodes
    libiberty
    binutils
    gnat_util
    gpr2
    templates-parser
    stable-sloc
  ];

  configurePhase = ''
    runHook preConfigure

    cd tools/gnatcov

    runHook postConfigure
  '';

  enableParallelBuilding = true;
  buildPhase = ''
    runHook preBuild

    make LIBRARY_TYPE=relocatable BUILD_MODE=prod C_SUPPORT=False all

    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall

    # Skip examples
    make LIBRARY_TYPE=relocatable BUILD_MODE=prod C_SUPPORT=False PREFIX=$out install-bin install-gnatcov_rts install-lib

    runHook postInstall
  '';
}
