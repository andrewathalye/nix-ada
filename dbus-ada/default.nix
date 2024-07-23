{ stdenv
, fetchzip

# Native
, gnat
, gprbuild

# Normal
, dbus
, dbus-glib
}:

stdenv.mkDerivation rec {
  pname = "dbus-ada";
  version = "0.6.2";

  src = fetchzip {
    url = "https://www.codelabs.ch/download/libdbusada-${version}.tar.bz2";
    sha256 = "sha256-C3v7yQXZE8l4+rfxmT7JKYn2Y6amhQyuVyDPib9Bxuo=";
  };

  # Set a variable to be constant to appease GNAT 13’s improved -Werror
  # Also allow empty arrays to be serialised
  patches = [ ./containers.patch ];

  nativeBuildInputs = [
    gnat
    gprbuild
  ];

  propagatedBuildInputs = [
    dbus
    dbus-glib
  ];

  buildPhase = ''
    runHook preBuild
    make NUM_CPUS=0
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall
    gprinstall -p -Pdbusada --prefix=$out -XVERSION=${version} --no-manifest
    runHook postInstall
  '';
}
