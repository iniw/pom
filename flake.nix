{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    fenix = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      fenix,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        overlays = [ fenix.overlays.default ];
        pkgs = import nixpkgs { inherit system overlays; };

        fenix-pkgs = import fenix { inherit system; };
        rust-toolchain = fenix-pkgs.fromToolchainFile {
          file = ./rust-toolchain.toml;
          sha256 = "sha256-FuOGHL+DbavyycfaDakNP1ANZ0qox3ha+v2/4MVI5YY=";
        };
      in
      {
        devShells.default = pkgs.mkShell {
          packages = [
            rust-toolchain
          ];
        };
      }
    );
}
