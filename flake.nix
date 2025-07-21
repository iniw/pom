{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    inputs:
    inputs.flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import inputs.nixpkgs { inherit system; };

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
            simpleCheck =
              {
                name,
                command,
                packages,
              }:
              {
                ${name} = pkgs.stdenv.mkDerivation {
                  inherit name;
                  src = inputs.self;

                  nativeBuildInputs = packages;

                  buildPhase = ''
                    ${command}
                    touch $out
                  '';
                };
              };

            cargoCheck =
              {
                name,
                command,
                packages,
              }:
              {
                ${name} = pkgs.stdenv.mkDerivation {
                  inherit name;
                  src = inputs.self;

                  # `cargoSetupHook` and `importCargoLock` allow fetching dependencies at build time, which some cargo commands (e.g: clippy) need to do.
                  # See: https://nixos.org/manual/nixpkgs/stable/#hooks
                  #      https://nixos.org/manual/nixpkgs/stable/#vendoring-of-dependencies
                  nativeBuildInputs = packages ++ [ pkgs.rustPlatform.cargoSetupHook ];
                  cargoDeps = pkgs.rustPlatform.importCargoLock {
                    lockFile = ./Cargo.lock;
                  };

                  buildPhase = ''
                    ${command}
                    touch $out
                  '';
                };
              };
          in
          simpleCheck {
            name = "typos";
            command = "typos";
            packages = [ pkgs.typos ];
          }
          // cargoCheck {
            name = "format";
            command = "cargo fmt --check";
            packages = rustToolchain;
          }
          // cargoCheck {
            # Lint and test in the same check to reuse the compiler cache.
            name = "lint+test";
            command = ''
              cargo clippy -- -Dwarnings
              cargo insta test
            '';
            packages = rustToolchain ++ [ pkgs.cargo-insta ];
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
            src = inputs.self;

            cargoLock.lockFile = ./Cargo.lock;

            # Only build and run tests for the specified package.
            cargoBuildFlags = "--package pom";
            cargoTestFlags = "--package pom";
          };
        };
      }
    );
}
