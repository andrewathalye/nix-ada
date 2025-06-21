{ stdenv
, fetchgit

# Build-time
, gnat
, gprbuild
, alire
, alire-index
, git
}:

let
   shared = import ./shared.nix { inherit fetchgit; };
in
stdenv.mkDerivation {
   pname = "wayland-ada-scanner";
   version = shared.version;

   src = shared.src;

   nativeBuildInputs = [
      gnat
      gprbuild
      alire
      alire-index
      git
   ];

   buildPhase = ''
      runHook preBuild
      cd wayland_ada_scanner

      alireBuild
      runHook postBuild
   '';

   installPhase = ''
      runHook preInstall
      HOME=${alire-index.out} alr install --prefix=$out
      runHook postInstall
   '';
}
