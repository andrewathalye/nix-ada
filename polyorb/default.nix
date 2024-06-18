{ stdenv
, fetchgit

# Build-time
, gnat
, gprbuild
, python3
}:

stdenv.mkDerivation {
  pname = "polyorb";
  version = "24.2-20240521";

  src = fetchGit {
    url = "https://github.com/AdaCore/PolyORB.git";
    ref = "master";
    rev = "3491a972ef22a5165a4bfd6c3231dc9fd0360b8f";
  };

  nativeBuildInputs = [
    gnat
    gprbuild
    python3
  ];

  configurePhase = ''
    runHook preConfigure
    ./configure --prefix=$out --with-gprbuild=gprbuild --enable-shared --disable-idlac-wrapper
    runHook postConfigure
  '';
}
