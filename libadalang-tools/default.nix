{ stdenv
, fetchzip
, gnat
, gprbuild
, glibc
, pkg-config
, libadalang
, templates-parser
, vss
}:

stdenv.mkDerivation {
  pname = "libadalang-tools";
  version = "23.0.0-20230802-git";
  
  src = fetchzip {
    url = "https://github.com/AdaCore/libadalang-tools/archive/c8692f0e126c1be284500aeab72ad24191a2f1ed.zip";
    sha256 = "MYy5SqzV2HaTdDGpU4SutUhLQGPn+3s6eu1bREiztow=";
  };
  
  nativeBuildInputs = [
    gprbuild
    gnat
    pkg-config
  ];

  propagatedBuildInputs = [
    templates-parser
    libadalang
    vss
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
