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
  version = "24.2-20240618-git";
  
  src = fetchGit {
    url = "https://github.com/AdaCore/libadalang-tools.git";
    ref = "master";
    rev = "2e14c0d664e7b636b2b06dc26d981319614bd01a";
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
