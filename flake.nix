{
   #inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
   inputs.nixpkgs.url = "github:NixOS/nixpkgs?rev=cc54fb41d13736e92229c21627ea4f22199fee6b";
   
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
