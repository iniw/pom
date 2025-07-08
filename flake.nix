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
            src = ./.;
            dontBuild = true;
            dontFixup = true;
            doCheck = true;
            nativeBuildInputs = [ rust-toolchain ];
            checkPhase = command;
            installPhase = ''mkdir "$out"'';
          };
      in
      {
        devShells.default = pkgs.mkShell {
          packages = [
            rust-toolchain
          ];
        };

        checks = {
          check = mkCheck "check" "cargo check";
          lint = mkCheck "clippy" "cargo clippy -- -Dwarnings";
          format = mkCheck "fmt" "cargo fmt --check";
        };

        packages = rec {
          pom = rustPlatform.buildRustPackage {
            name = "pom";
            src = ./.;
            cargoLock.lockFile = ./Cargo.lock;
          };

          default = pom;
        };
      }
    );
}
