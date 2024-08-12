{ stdenv
, gnat
, fetchzip
, gprbuild
, gnatcoll-core
, libadalang
, zlib
, libbfd
, libopcodes
, libiberty
, binutils
, gnat_util
}:

stdenv.mkDerivation rec {
  pname = "gnatcoverage";
  version = "24.2-20240731";
  
  src = fetchGit {
    url = "https://github.com/AdaCore/gnatcoverage.git";
    ref = "master";
    rev = "0333810f95f49619681c6a72ce2e554ed0181887";
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
