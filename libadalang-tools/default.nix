{ stdenv
, fetchgit
, gnat
, gprbuild
, glibc
, pkg-config
, libadalang
, templates-parser
, vss
}:

stdenv.mkDerivation rec {
  pname = "libadalang-tools";
  version = "24.2";
  
  src = fetchGit {
    url = "https://github.com/AdaCore/libadalang-tools.git";
    ref = version;
    rev = "0b842b579a68a2302a2fdc6b59688e2ce7b0d03b";
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
