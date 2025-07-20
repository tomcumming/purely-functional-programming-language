{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.11";
    # unixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };
  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages."${system}";
      # upkgs = unixpkgs.legacyPackages."${system}";
    in
    {
      devShells."${system}".default = pkgs.mkShell {
        packages = [
          (pkgs.haskell-language-server.override { supportedGhcVersions = [ "9122" ]; })
          pkgs.haskell.compiler.ghc9122
          pkgs.cabal-install
          pkgs.ormolu
        ];
      };
      formatter."${system}" = pkgs.nixpkgs-fmt;
    };
}
