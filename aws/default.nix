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

  propagatedBuildInputs = [
    xmlada
    gnatcoll-core
    openssl
    zlib
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

  # Remove invalid syntax from GPR file.
  # This confuses ada_language_server
  installPhase = ''
    runHook preInstall
    make install
    sed -i 's/for implementation_exceptions.*;//g' $out/share/gpr/aws.gpr
    runHook postInstall
  '';
}
