{ stdenv
, fetchgit
, gnat
, gprbuild
, libadalang-tools
, vss-extra
}:

stdenv.mkDerivation rec {
  pname = "lal-refactor";
  version = "26.0.0-20260105";
  
  src = fetchGit {
    url = "https://github.com/AdaCore/lal-refactor.git";
    ref = "edge";
    rev = "2e1a0b61fc228efa3e2c6aa9eb3a0388d372361c";
  };

  patches = [ ./extract_variable.patch ];

  nativeBuildInputs = [
    gprbuild
    gnat
  ];

  buildInputs = [
    libadalang-tools
    vss-extra
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
