{ stdenv
, fetchgit
, gnat
, gprbuild
, pkg-config
, libadalang
, templates-parser
, vss-text
}:

stdenv.mkDerivation rec {
  pname = "libadalang-tools";
  version = "26.0.0-20250902";
  
  src = fetchGit {
    url = "https://github.com/AdaCore/libadalang-tools.git";
    ref = "v26.0.0";
    rev = "8a506bb4f65ec93da749364a767237feca1c7eba";
  };
  
  nativeBuildInputs = [
    gprbuild
    gnat
    pkg-config
  ];

  buildInputs = [
    vss-text
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
