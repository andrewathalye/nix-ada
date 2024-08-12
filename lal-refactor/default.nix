{ stdenv
, fetchgit
, gnat
, gprbuild
, libadalang-tools
, vss
}:

stdenv.mkDerivation rec {
  pname = "lal-refactor";
  version = "24.2-20240613-git";
  
  src = fetchGit {
    url = "https://github.com/AdaCore/lal-refactor.git";
    ref = "edge";
    rev = "9a09c7f910fb5a229741cb41099e97625ea6db1b";
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
