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

                # Manually create the $out folder to make the derivation build successfully - nix derivations must produce an output.
                # See: https://github.com/NixOS/nixpkgs/issues/16182
                buildPhase = ''
                  ${command}
                  mkdir "$out"
                '';
              };
          in
          {
            format = check "format" "cargo fmt --check" [ rustToolchain ];
            lint = check "lint" "cargo clippy -- -Dwarnings" [ rustToolchain ];
            test = check "test" "cargo insta test" [
              rustToolchain
              pkgs.cargo-insta
            ];
            typos = check "typos" "typos" [ pkgs.typos ];
          };

        devShells.default = pkgs.mkShell {
          packages = rustToolchain ++ [
            # Better DX to run and review the snapshot tests.
            pkgs.cargo-insta
          ];
        };

        packages = rec {
          default = pom;

          pom = pkgs.rustPlatform.buildRustPackage {
            name = "pom";
            src = self;

            cargoLock.lockFile = ./Cargo.lock;

            # Only build and run tests for the specified package.
            cargoBuildFlags = "--package pom";
            cargoTestFlags = "--package pom";
          };
        };
      }
    );
}
