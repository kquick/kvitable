{
  # $ nix develop
  # $ nix build
  # $ nix develop .#kvitable.ghc884.env
  # $ nix build  .#kvitable.ghc884.default

  description = "Haskell Key/Value Indexed Table library";

  nixConfig.bash-prompt-suffix = "kvitable} ";

  inputs = {
    # nixpkgs.url = github:nixos/nixpkgs/nixpkgs-unstable;
    nixpkgs.url = github:nixos/nixpkgs/22.05;
    levers = {
      type = "github";
      owner = "kquick";
      repo = "nix-levers";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    html-parse-src = { flake = false; url = github:bgamari/html-parse; };
  };

  outputs = { self, nixpkgs, levers
            , html-parse-src
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
              inherit html-parse;
            };

            html-parse = mkHaskell "html-parse" html-parse-src {
              adjustDrv = args: drv:
                let ghcv = args.ghcver or "ghc8104"; in
                if builtins.compareVersions pkgs.haskell.compiler."${ghcv}".version "9.0" < 0
                then drv
                else pkgs.haskell.lib.doJailbreak drv;
            };
          });
      };
}
