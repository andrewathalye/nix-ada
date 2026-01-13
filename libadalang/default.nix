{ stdenv
, fetchgit
, gnat
, gprbuild
, pkg-config
, gnatPackages
, langkit-support
, langkit
, langkit-lktlang
, python3
}:

with gnatPackages;
stdenv.mkDerivation rec {
  pname = "libadalang";
  version = "26.0.0-20260108";
  
  src = fetchGit {
    url = "https://github.com/AdaCore/libadalang.git";
    ref = "master";
    rev = "b232352be79d5eec9a1c72727634b19a52d62e07";
  };

  nativeBuildInputs = [
    gprbuild
    gnat
    pkg-config
    python3
  ];

  propagatedBuildInputs = [
    gnatcoll-core
    gnatcoll-gmp
    gnatcoll-iconv
    langkit
    langkit-lktlang
    langkit-support
    gpr2
  ];

  configurePhase = ''
    runHook preConfigure

    LD_LIBRARY_PATH=${langkit-lktlang.out}/lib python -m langkit.scripts.lkm generate
    runHook postConfigure
  '';

  buildPhase = ''
    runHook preBuild
    python -m langkit.scripts.lkm build --disable-build-warnings
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall
    python -m langkit.scripts.lkm install $out
    runHook postInstall
  '';
}
