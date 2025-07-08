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

        rustPlatform = pkgs.makeRustPlatform {
          cargo = rust-toolchain;
          rustc = rust-toolchain;
        };

        mkCheck =
          name: command:
          pkgs.stdenv.mkDerivation {
            inherit name;
            src = self;
            buildInputs = [ rust-toolchain ];
            buildPhase = ''
              ${command}
              mkdir "$out"
            '';
          };
      in
      {
        devShells.default = pkgs.mkShell {
          packages = [
            rust-toolchain
          ];
        };

        checks = {
          lint = mkCheck "clippy" "cargo clippy -- -Dwarnings";
          format = mkCheck "fmt" "cargo fmt --check";
          package = self.packages.${system}.default;
        };

        packages = rec {
          pom = rustPlatform.buildRustPackage {
            name = "pom";
            src = self;
            cargoLock.lockFile = ./Cargo.lock;
          };
          default = pom;
        };
      }
    );
}
