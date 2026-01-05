{
  # $ nix develop
  # $ nix build
  # $ nix develop .#kvitable.ghc884.env
  # $ nix build  .#kvitable.ghc884.default

  description = "Haskell Key/Value Indexed Table library";

  nixConfig.bash-prompt-suffix = "kvitable} ";

  inputs = {
    nixpkgs.url = github:nixos/nixpkgs/nixpkgs-unstable;
    levers = {
      type = "github";
      owner = "kquick";
      repo = "nix-levers";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    html-parse-src = {
      url = "github:bgamari/html-parse";
      # Needed because the GHC 9.4 build in nixpkgs/23.11 has html-parse
      # 0.2.0.2 marked as broken.
      flake = false;
    };
    named-text = {
      url = "github:kquick/named-text";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.levers.follows = "levers";
      inputs.sayable.follows = "sayable";
      inputs.parameterized-utils-src.follows = "parameterized-utils-src";
    };
    parameterized-utils-src = {
      # Lock to revision 6cd1f32 because the subsequent changes replace lens with
      # microlens-pro, and the latter was only introduced in Feb 2024 (circa GHC
      # 9.8.3), so it's not available for older nixpkgs configurations.  As of
      # 2025 Dec, there are no functional changes in parameterized-utils, just
      # dependency changes, and there is intent to remove the microlens-pro
      # dependency from parameterized-utils, so freeze this until that occurs.
      url = "github:GaloisInc/parameterized-utils/6cd1f32";
      flake = false;
    };
    sayable = {
      url = "github:kquick/sayable";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.levers.follows = "levers";
    };
  };

  outputs = { self, nixpkgs
            , levers
            , html-parse-src
            , named-text
            , sayable
            , parameterized-utils-src
            }:
    rec
      {
      devShells = levers.haskellShells
        { inherit nixpkgs;
          flake = self;
          defaultPkg = "kvitable";
          # additionalPackages = pkgs.haskell.packages.ghc8107.profiteur
        };

        packages = levers.eachSystem (system:
          let mkHaskell = levers.mkHaskellPkg {inherit nixpkgs system;};
              pkgs = import nixpkgs { inherit system; };
          in rec {
            default = kvitable;
            kvitable = mkHaskell "kvitable" self {
              inherit html-parse named-text sayable;
            };
            html-parse = mkHaskell "html-parse" html-parse-src {};
          });
      };
}
