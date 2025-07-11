{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };

        rustToolchain = with pkgs; [
          cargo
          clippy
          rust-analyzer
          rustc
          rustfmt
        ];
      in
      {
        checks =
          let
            check =
              name: command: inputs:
              pkgs.stdenv.mkDerivation {
                inherit name;
                src = self;

                # `cargoSetupHook` and `importCargoLock` allow fetching dependencies at build time, which some cargo commands (e.g: clippy) need to do.
                # See: https://nixos.org/manual/nixpkgs/stable/#hooks
                #      https://nixos.org/manual/nixpkgs/stable/#vendoring-of-dependencies
                nativeBuildInputs = [ pkgs.rustPlatform.cargoSetupHook ] ++ inputs;
                cargoDeps = pkgs.rustPlatform.importCargoLock {
                  lockFile = ./Cargo.lock;
                };

                # Create the output folder to make the derivation succeed because the command won't produce any output.
                buildPhase = ''
                  ${command}
                  mkdir "$out"
                '';
              };
          in
          {
            format = check "format" "cargo fmt --check" [ rustToolchain ];
            lint = check "lint" "cargo clippy -- -Dwarnings" [ rustToolchain ];
            typos = check "typos" "typos" [ pkgs.typos ];
          }
          # Check that every package builds.
          # `buildRustPackage` also runs tests for the package in the checkPhase, so this also makes sure that all tests succeed.
          // self.packages.${system};

        devShells.default = pkgs.mkShell {
          packages = rustToolchain ++ [
            # Better DX to run and review the snapshot tests.
            pkgs.cargo-insta
          ];
        };

        packages =
          let
            package =
              name:
              pkgs.rustPlatform.buildRustPackage {
                inherit name;
                src = self;

                cargoLock.lockFile = ./Cargo.lock;

                # Only build and run tests for the specified package.
                cargoBuildFlags = "--package ${name}";
                cargoTestFlags = "--package ${name}";
              };
          in
          {
            pom = package "pom";
            pom-lexer = package "pom-lexer";
            pom-parser = package "pom-parser";
            pom-utils = package "pom-utils";
          };
      }
    );
}
