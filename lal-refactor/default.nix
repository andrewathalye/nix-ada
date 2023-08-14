{ stdenv
, fetchzip
, gnat
, gprbuild
, glibc
, libadalang-tools
, vss
}:

stdenv.mkDerivation {
  pname = "lal-refactor";
  version = "20230807-git";
  
  src = fetchzip {
    url = "https://github.com/AdaCore/lal-refactor/archive/dcc1d260a3c3186394fbc794debdc4333b1275a3.zip";
    sha256 = "QiWHx5l7j5EsmUtZaZg1Ufh8LZNocM/cAC8LcUKlldU=";
  };

  nativeBuildInputs = [
    gprbuild
    gnat
  ];

  buildInputs = [
    libadalang-tools
    vss
  ];

  configurePhase = ''
    runHook preConfigure
    sed -i '1s/^/with "vss_text";\n/' gnat/lal_refactor.gpr
    runHook postConfigure
  '';

  commonFlags = "-XLIBRARY_TYPE=relocatable -XLAL_REFACTOR_BUILD_MODE=prod -XLAL_TOOLS_BUILD=default";
  buildPhase = ''
    runHook preBuild
    gprbuild $commonFlags -Pgnat/lal_refactor.gpr -j0
    gprbuild $commonFlags -Pgnat/lal_refactor_driver.gpr -j0
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall
    gprinstall $commonFlags -p -Pgnat/lal_refactor.gpr --prefix=$out
    gprinstall $commonFlags -p -Pgnat/lal_refactor_driver.gpr --prefix=$out
    runHook postInstall
  '';
}
