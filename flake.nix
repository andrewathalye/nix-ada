{
   #inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
   inputs.nixpkgs.url = "github:NixOS/nixpkgs?rev=b2485d56967598da068b5a6946dadda8bfcbcd37";

   inputs.dbus-ada-flake = {
     url = "github:andrewathalye/dbus-ada";
     inputs.nixpkgs.follows = "nixpkgs";
   };

   outputs = { self, nixpkgs, dbus-ada-flake }:
   let
      system = "x86_64-linux";

      pkgs = import nixpkgs { inherit system; overlays = (import ./overlays.nix {}); };

      dbus-ada = dbus-ada-flake.packages.${system}.default;

      nix-ada = import ./default.nix { inherit pkgs dbus-ada; };

   in {
      packages.${system} = nix-ada;

      devShells.${system}.default = import ./shell.nix { inherit nix-ada; };
   };
}
