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
          sha256 = "sha256-KrfXTnUMDG3QBuAb2PQGrRigTdJep1omsY63UBRhCf8=";
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
