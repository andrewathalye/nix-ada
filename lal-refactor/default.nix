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
  version = "24.2";
  
  src = fetchzip {
    url = "https://github.com/AdaCore/lal-refactor/archive/refs/heads/24.2.zip";
    sha256 = "sha256-nrGkALirATEjCANai1xoxpovryQ/wtxv2dRmATnd2us=";
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
