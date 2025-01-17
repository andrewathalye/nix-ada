{ stdenv
, fetchgit
, gnat
, gprbuild
, pkg-config
, libadalang
, templates-parser
, vss
}:

stdenv.mkDerivation rec {
  pname = "libadalang-tools";
  version = "25.0.0-20250114";
  
  src = fetchGit {
    url = "https://github.com/AdaCore/libadalang-tools.git";
    ref = "master";
    rev = "1dcb3f7a34b11cc11bf3f7de64d482daef731dae";
  };
  
  nativeBuildInputs = [
    gprbuild
    gnat
    pkg-config
  ];

  buildInputs = [
    vss
  ];

  propagatedBuildInputs = [
    templates-parser
    libadalang
  ];

  dontConfigure = true;

  options = "-XLIBRARY_TYPE=relocatable -XLALTOOLS_BUILD_MODE=prod";

  # Build manually because the Makefile tries to use incompatible syntax
  buildPhase = ''
    runHook preBuild

    gprbuild $options -Psrc/lal_tools.gpr -j0
    gprbuild $options -Psrc/build.gpr -j0

    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall

    gprinstall $options --prefix=$out --build-var=LIBRARY_TYPE --build-var=LAL_TOOLS_BUILD -Psrc/lal_tools.gpr -p -f
    gprinstall $options --prefix=$out --build-var=LIBRARY_TYPE --build-var=LAL_TOOLS_BUILD -Psrc/build.gpr -p -f

    runHook postInstall
  '';
}
