{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
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
            cargoCheck =
              name: command: buildInputs:
              pkgs.stdenv.mkDerivation {
                inherit name;
                src = inputs.self;

                # `cargoSetupHook` and `importCargoLock` allow fetching dependencies at build time, which some cargo commands (e.g: clippy) need to do.
                # See: https://nixos.org/manual/nixpkgs/stable/#hooks
                #      https://nixos.org/manual/nixpkgs/stable/#vendoring-of-dependencies
                nativeBuildInputs = [ pkgs.rustPlatform.cargoSetupHook ] ++ buildInputs;
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

            simpleCheck =
              name: command: buildInputs:
              pkgs.stdenv.mkDerivation {
                inherit name;
                src = inputs.self;
                nativeBuildInputs = buildInputs;
                buildPhase = ''
                  ${command}
                  mkdir "$out"
                '';
              };
          in
          {
            format = cargoCheck "format" "cargo fmt --check" [ rustToolchain ];
            lint = cargoCheck "lint" "cargo clippy -- -Dwarnings" [ rustToolchain ];
            test = cargoCheck "test" "cargo insta test" [
              rustToolchain
              pkgs.cargo-insta
            ];
            typos = simpleCheck "typos" "typos" [ pkgs.typos ];
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
