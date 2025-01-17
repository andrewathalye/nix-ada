{
   #inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
   inputs.nixpkgs.url = "github:NixOS/nixpkgs?rev=310c5c4d3fd457d80365f52c71ac80b9d54a7cc4";

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
