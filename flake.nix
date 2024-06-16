{
   inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
   
   outputs = { self, nixpkgs }:
   let
      system = "x86_64-linux";

      pkgs = import nixpkgs { system = system; overlays = (import ./overlays.nix {}); };
      nix-ada = import ./default.nix { pkgs = pkgs; };
   in {
      packages.${system} = nix-ada;
      devShells.${system}.default = import ./shell.nix { nix-ada = nix-ada; };
   };
}
