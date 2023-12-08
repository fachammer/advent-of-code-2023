{
  description = "A very basic flake";

  inputs = { nixpkgs.url = "github:nixos/nixpkgs/23.11"; };

  outputs = { self, nixpkgs }:
    let pkgs = nixpkgs.legacyPackages.aarch64-darwin;
    in {
      devShells.aarch64-darwin.default = pkgs.mkShell {
        buildInputs = with pkgs; [ scala_3 sbt nil nixfmt jdk visualvm ];
      };
    };
}
