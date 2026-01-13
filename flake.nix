{
   #inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
   inputs.nixpkgs.url = "github:NixOS/nixpkgs?rev=d351d0653aeb7877273920cd3e823994e7579b0b";

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
