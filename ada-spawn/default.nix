{ lib
, stdenv
, fetchzip
, gnat
, gprbuild
, glibc
, glib
, glibSupport ? false, gtkada
}:

let
  inherit (lib) optionalString optionals;
in
stdenv.mkDerivation {
  pname = "ada-spawn" + optionalString glibSupport "-glib";
  version = "23.0.0.20230518-git";
  
  src = fetchzip {
    url = "https://github.com/AdaCore/spawn/archive/2642bd6d7d99e92efab5328b61bb5024107726ac.zip";
    sha256 = "9y5umxeyeyjmthVO9+2x6v2zg8hYPakIKt1WKpT3kHE=";
  };
  
  nativeBuildInputs = [
    gprbuild
    gnat
  ];

  buildInputs = []
  ++ optionals glibSupport [ glib gtkada ];

  propagatedBuildInputs = []
  ++ optionals glibSupport [ glib gtkada ];

  dontConfigure = true;

  buildPhase = ''
    runHook preBuild
    gprbuild -j0 -P gnat/spawn'' + optionalString glibSupport "_glib" + ''

    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall
    gprinstall --prefix=$out -p -P gnat/spawn'' + optionalString glibSupport "_glib" + ''

    runHook postInstall
  '';
}
