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
  version = "0.7.0";

  src = ./.;

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
    gprbuild -Pdbusada -j0 -XVERSION=${version}
    runHook postBuild
  '';

  checkPhase = ''
    runHook preCheck
    make tests
    runHook postCheck 
  '';

  installPhase = ''
    runHook preInstall
    gprinstall -p -Pdbusada --prefix=$out -XVERSION=${version} --no-manifest
    runHook postInstall
  '';
}
