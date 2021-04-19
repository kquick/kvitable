{
  # $ nix develop
  # $ nix build
  # $ nix develop .#kvitable.ghc884.env
  # $ nix build  .#kvitable.ghc884.default

  description = "Haskell Key/Value Indexed Table library";

  nixConfig.bash-prompt-suffix = "kvitable} ";

  inputs = {
    nixpkgs.url = github:nixos/nixpkgs/nixpkgs-unstable;
    # nixpkgs.url = github:nixos/nixpkgs/20.09;
    levers = {
      type = "github";
      owner = "kquick";
      repo = "nix-levers";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    html-parse-src = { flake = false; url = github:bgamari/html-parse; };
    prettyprinter-src = { flake = false; url = github:quchen/prettyprinter?dir=prettyprinter; };
  };

  outputs = { self, nixpkgs, levers
            , html-parse-src
            , prettyprinter-src
            }: rec
      {
        defaultPackage = levers.eachSystem (s:
          self.packages.${s}.kvitable.default);

        devShell = levers.eachSystem (s: defaultPackage.${s}.env);

        packages = levers.eachSystem (system:
          let mkHaskell = levers.mkHaskellPkg {
                inherit nixpkgs system;
              };
              pkgs = import nixpkgs { inherit system; };
          in rec {

            kvitable = mkHaskell "kvitable" self {
              inherit html-parse prettyprinter;
            };

            html-parse = mkHaskell "html-parse" html-parse-src {
              adjustDrv = args: drv:
                let ghcv = args.ghcver or "ghc8104"; in
                if builtins.compareVersions (builtins.trace ghcv pkgs.haskell.compiler."${ghcv}".version) "9.0" < 0
                then drv
                else pkgs.haskell.lib.doJailbreak drv;
            };

            prettyprinter = mkHaskell "prettyprinter"
              "${prettyprinter-src}/prettyprinter" {
                adjustDrv = args: drv:
                  pkgs.haskell.lib.overrideCabal
                    (pkgs.haskell.lib.dontCheck
                      (pkgs.haskell.lib.appendConfigureFlags drv
                        [ "--extra-include-dirs=${prettyprinter-macros}" ]))
                    (old: {
                      # The LICENSE.md in prettyprinter is a symlink
                      # to the directory above it, but the src
                      # specification will lose it, so create an
                      # explicit copy instead.
                      preConfigure = ''
                              rm /build/prettyprinter/LICENSE.md
                              cp ${prettyprinter-src}/LICENSE.md /build/prettyprinter/LICENSE.md
                            '';
                          });
                };
            prettyprinter-macros = pkgs.stdenv.mkDerivation {
              pname = "prettyprinter-macros";
              version = "1.7.0";
              phases = [ "installPhase" ];
              src = "${prettyprinter-src}/aux";
              installPhase = ''
                mkdir $out
                cp ${prettyprinter-src}/aux/version-compatibility-macros.h $out/
              '';
            };
          });
      };
}
