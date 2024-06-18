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
    rev = "1991404ded15a99a79745569017bb42594be9eca";
  };

  nativeBuildInputs = [
    gnat
    gprbuild
  ];
}
