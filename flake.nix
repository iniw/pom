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
      flake-utils,
      fenix,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };

        toolchain = fenix.packages.${system}.complete.withComponents [
          "cargo"
          "clippy"
          "rust-analyzer"
          "rust-src"
          "rustc"
          "rustfmt"
        ];
      in
      {
        checks =
          let
            mkCheck =
              name: command:
              pkgs.stdenv.mkDerivation {
                inherit name;
                src = self;
                buildInputs = [ toolchain ];
                buildPhase = ''
                  ${command}
                  mkdir "$out"
                '';
              };
          in
          {
            format = mkCheck "fmt" "cargo fmt --check";
            lint = mkCheck "clippy" "cargo clippy -- -Dwarnings";
            package = self.packages.${system}.default;
          };

        devShells.default = pkgs.mkShell {
          packages = [
            toolchain
          ];
        };

        packages.default =
          let
            rustPlatform = pkgs.makeRustPlatform {
              cargo = toolchain;
              rustc = toolchain;
            };
          in
          rustPlatform.buildRustPackage {
            name = "pom";
            src = self;
            cargoLock.lockFile = ./Cargo.lock;
          };
      }
    );
}
