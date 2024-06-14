{ stdenv
, fetchzip

# Build-time
, gnat
, gprbuild
, alire
, alire-index
, git
}:

let
   shared = import ./shared.nix {fetchzip = fetchzip;};
in
stdenv.mkDerivation {
   pname = "wayland-ada-scanner";
   version = shared.version;

   src = shared.src;

   nativeBuildInputs = [
      gnat
      gprbuild
      alire
      git
   ];

   buildPhase = ''
      runHook preBuild
      cd wayland_ada_scanner

      # Work around Alire bug in Nix builds
      HOME=${alire-index.out} alr build

      runHook postBuild
   '';

   installPhase = ''
      runHook preInstall
      HOME=${alire-index.out} alr install --prefix=$out
      runHook postInstall
   '';
}
