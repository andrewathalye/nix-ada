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
      git
   ];

   buildPhase = ''
      runHook preBuild
      cd wayland_ada_scanner

      # TODO workaround lack of Alire support for vendored indexes / airgapped builds
      # TODO this is an Alire bug, reported as #1769 on alire-project/alire
      cp -rL ${alire-index.out}/.config /tmp/.config
      chmod -R u+w /tmp/.config
      HOME=/tmp alr build

      runHook postBuild
   '';

   installPhase = ''
      runHook preInstall
      HOME=${alire-index.out} alr install --prefix=$out
      runHook postInstall
   '';
}
