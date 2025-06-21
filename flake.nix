{
   #inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
   inputs.nixpkgs.url = "github:NixOS/nixpkgs?rev=b2485d56967598da068b5a6946dadda8bfcbcd37";

   outputs = { self, nixpkgs }:
   let
      system = "x86_64-linux";

      pkgs = import nixpkgs { inherit system; overlays = (import ./overlays.nix {}); };

      nix-ada = import ./default.nix { inherit pkgs; };

   in {
      packages.${system} = nix-ada;

      devShells.${system}.default = import ./shell.nix { inherit nix-ada; };
   };
}
