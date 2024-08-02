{
   #inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
   inputs.nixpkgs.url = "github:NixOS/nixpkgs?rev=cc54fb41d13736e92229c21627ea4f22199fee6b";

   inputs.dbus-ada-flake = {
     url = "github:andrewathalye/dbus-ada";
#     url = "git+file:///home/andrew/src/ada/dbus-ada/";
     inputs.nixpkgs.follows = "nixpkgs";
   };
   
   outputs = { self, nixpkgs, dbus-ada-flake }:
   let
      system = "x86_64-linux";

      pkgs = import nixpkgs { inherit system; overlays = (import ./overlays.nix {}); };

      # External Packages
      dbus-ada = dbus-ada-flake.packages.${system}.default.override { inherit (pkgs) gnat gprbuild; };

      nix-ada = import ./default.nix { inherit pkgs dbus-ada; };
   in {
      packages.${system} = nix-ada;

      devShells.${system}.default = import ./shell.nix { inherit nix-ada; };
   };
}
