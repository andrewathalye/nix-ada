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
  version = "25.0.0";

  src = fetchGit {
    url = "https://github.com/AdaCore/prettier-ada";
    ref = "25.0";
    rev = "26b003699e2ef31baa9a95ce9313b4bde6efe701";
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
