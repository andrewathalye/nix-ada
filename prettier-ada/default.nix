{ stdenv
, fetchgit

# Build-time
, gnat
, gprbuild

# Dependencies
, gnatcoll-core
, vss
}:

stdenv.mkDerivation {
  pname = "prettier-ada";
  version = "20240618-git";

  src = fetchGit {
    url = "https://github.com/AdaCore/prettier-ada";
    ref = "main";
    rev = "4635f872b1992376f89735d9dd987ad93648fe96";
  };

  nativeBuildInputs = [
    gnat
    gprbuild
  ];

  propagatedBuildInputs = [
    gnatcoll-core
    vss
  ];

  dontConfigure = true;

  buildPhase = ''
    runHook preBuild
    make lib
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall
    PREFIX=$out make install
    runHook postInstall
  '';
}
