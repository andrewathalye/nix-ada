{ stdenv
, fetchzip

# Build-time
, gnat
, gprbuild

# Dependencies
, xmlada
, openssl
, zlib
, gnatcoll-core
}:

stdenv.mkDerivation {
  pname = "aws";
  version = "23.0.0";

  src = fetchzip {
    url = "https://github.com/AdaCore/aws/releases/download/v23.0.0/aws-23.0.0.zip";
    sha256 = "6vVHBIfkK+iHZX68wrz2dKy9OwBjkgZcWkoekz0sXdo=";
  };

  nativeBuildInputs = [
    gnat
    gprbuild
  ];

  buildInputs = [
    zlib
  ];

  propagatedBuildInputs = [
    xmlada
    gnatcoll-core
    xmlada
    openssl
  ];

  configurePhase = ''
    runHook preConfigure
    make prefix=$out ENABLE_SHARED=true SOCKET=openssl DEBUG=false setup
    runHook postConfigure
  '';

  buildPhase = ''
    runHook preBuild
    make build
    runHook postBuild
  '';
}
