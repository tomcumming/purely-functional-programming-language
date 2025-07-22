{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.05";
  };
  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages."${system}";
    in
    {
      devShells."${system}".default = pkgs.mkShell {
        packages = [
          pkgs.zlib

          pkgs.haskell.compiler.ghc9122
          pkgs.cabal-install
        ];

        shellHook = ''
          export BIN_DIR="$PWD/.bin"
          export PATH="$BIN_DIR:$PATH"
        '';
      };
      formatter."${system}" = pkgs.nixpkgs-fmt;
    };
}
