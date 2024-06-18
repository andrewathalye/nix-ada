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
  patches = [ ./d_bus-arguments-containers.adb.patch ];

  nativeBuildInputs = [
    gnat
    gprbuild
  ];

  buildInputs = [
    dbus
    dbus-glib
  ];

  installPhase = ''
    runHook preInstall
    make PREFIX=$out install
    runHook postInstall
  '';
}
