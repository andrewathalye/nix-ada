{ stdenv
, fetchzip

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
   shared = import ./shared.nix {fetchzip = fetchzip;};
in
stdenv.mkDerivation rec {
   pname = "wayland-ada";
   version = shared.version;

   src = shared.src;

   # Custom alire.toml to avoid dependency issues
   wayland-ada-alire = ./wayland-ada-alire.toml;

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
      ln -s ${wayland-ada-alire} alire.toml

      # Workaround for Alire bug
      HOME=${alire-index.out} alr build
      runHook postBuild
   '';

   installPhase = ''
      runHook preInstall
      gprinstall -Pwayland_client_ada -p --prefix=$out
      runHook postInstall
   '';
}
