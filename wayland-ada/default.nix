{ stdenv
, fetchgit

# Build-time
, gnat
, gprbuild
, alire
, git

# Run-time
, wayland

# Our packages
, alire-index
, wayland-ada-scanner
}:

let
   shared = import ./shared.nix { inherit fetchgit; };
in
stdenv.mkDerivation rec {
   pname = "wayland-ada";
   version = shared.version;

   src = shared.src;

   # Custom alire.toml to avoid dependency issues
   alire_toml = ./wayland_client_ada/alire.toml;

   # Enable shared library over static
   patches = [ ./wayland_client_ada/wayland_client_ada.gpr.patch ];

   nativeBuildInputs = [
      gnat
      gprbuild
      wayland-ada-scanner
      alire
      git
   ];

   propagatedBuildInputs = [ wayland ];

   buildPhase = ''
      runHook preBuild
      cd wayland_client_ada
      rm alire.toml
      ln -s ${alire_toml} alire.toml

      alireBuild
      runHook postBuild
   '';

   installPhase = ''
      runHook preInstall
      gprinstall -Pwayland_client_ada -p --prefix=$out
      runHook postInstall
   '';
}
