{ stdenv
, fetchgit

# Build-time
, gnat
, gprbuild
}:

stdenv.mkDerivation rec {
  pname = "florist";
  version = "24.2";

  src = fetchGit {
    url = "https://github.com/AdaCore/florist.git";
    ref = version;
  };

  nativeBuildInputs = [
    gnat
    gprbuild
  ];
}
