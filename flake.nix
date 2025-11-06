{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    crane.url = "github:ipetkov/crane";
  };

  outputs =
    inputs:
    inputs.flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import inputs.nixpkgs { inherit system; };
        crane = inputs.crane.mkLib pkgs;
      in
      {
        devShells.default = crane.devShell {
          checks = inputs.self.checks.${system};

          packages = with pkgs; [
            rust-analyzer
            cargo-insta
          ];
        };

        checks =
          let
            cargoArtifacts = crane.buildDepsOnly {
              src = crane.cleanCargoSource ./.;
              strictDeps = true;
            };
          in
          {
            build-and-test = crane.buildPackage {
              src = pkgs.lib.fileset.toSource {
                root = ./.;
                fileset = pkgs.lib.fileset.unions [
                  (crane.fileset.commonCargoSources ./.)
                  # Snapshot expectations
                  (pkgs.lib.fileset.fileFilter (file: file.hasExt "snap") ./.)
                ];
              };

              inherit cargoArtifacts;
            };

            clippy = crane.cargoClippy {
              src = crane.cleanCargoSource ./.;

              cargoClippyExtraArgs = "--all-targets -- --deny warnings";

              inherit cargoArtifacts;
            };

            fmt = crane.cargoFmt {
              src = crane.cleanCargoSource ./.;
            };

            toml-fmt = crane.taploFmt {
              src = pkgs.lib.sources.sourceFilesBySuffices ./. [ ".toml" ];
            };

            typos = pkgs.stdenvNoCC.mkDerivation {
              name = "typos";
              nativeBuildInputs = [ pkgs.typos ];

              src = ./.;

              buildPhase = ''
                typos
                touch $out
              '';
            };
          };
      }
    );
}
