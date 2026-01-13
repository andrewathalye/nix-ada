{ stdenv
, fetchgit

# Build-time
, gnat
, gprbuild

# Dependencies
, gnatPackages
, openssl
, zlib
}:

with gnatPackages;
stdenv.mkDerivation {
  pname = "aws";
  version = "24.2-20240607";

  src = fetchGit {
    url = "https://github.com/AdaCore/aws.git";
    submodules = true;
    ref = "master";
    rev = "8e5dfe946b1334c93f0efb13bb4dff171a480cd3";
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
    make prefix=$out ENABLE_SHARED=true SOCKET=openssl ZLIB=true DEBUG=false setup
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
