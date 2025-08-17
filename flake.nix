{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.05";
    unixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };
  outputs = { self, nixpkgs, unixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages."${system}";
      upkgs = unixpkgs.legacyPackages."${system}";
    in
    {
      devShells."${system}".default = pkgs.mkShell {
        packages = [
          pkgs.zlib

          (upkgs.haskell-language-server.override { supportedGhcVersions = [ "9122" ]; })
          upkgs.haskell.compiler.ghc9122
          upkgs.cabal-install
          pkgs.ormolu
        ];
      };
      formatter."${system}" = pkgs.nixpkgs-fmt;
    };
}
