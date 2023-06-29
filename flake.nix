{
  # $ nix develop
  # $ nix build
  # $ nix develop .#kvitable.ghc884.env
  # $ nix build  .#kvitable.ghc884.default

  description = "Haskell Key/Value Indexed Table library";

  nixConfig.bash-prompt-suffix = "kvitable} ";

  inputs = {
    # nixpkgs.url = github:nixos/nixpkgs/nixpkgs-unstable;
    nixpkgs.url = github:nixos/nixpkgs/23.05;
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
        devShell = levers.eachSystem (s: packages.${s}.default.env);

        packages = levers.eachSystem (system:
          let mkHaskell = levers.mkHaskellPkg {
                inherit nixpkgs system;
              };
              pkgs = import nixpkgs { inherit system; };
              wrap = levers.pkg_wrapper system pkgs;
              haskellAdj = drv:
                with (pkgs.haskell).lib;
                dontHaddock (dontCheck (dontBenchmark (drv)));
          in rec {
            default = kvitable;
            TESTS = wrap "kvitable-TESTS" [ kvitable-test ];
            DOC = wrap "kvitable-DOC" [ kvitable-doc ];

            kvitable = mkHaskell "kvitable" self {
              inherit html-parse;
              adjustDrv = args: haskellAdj;
            };
            kvitable-test = mkHaskell "kvitable-test" self {
              inherit html-parse;
              adjustDrv = args: drv: pkgs.haskell.lib.doCheck (haskellAdj drv);
            };
            kvitable-doc = mkHaskell "kvitable-doc" self {
              inherit html-parse;
              adjustDrv = args: drv:
                with pkgs.haskell.lib; dontCheck (dontBenchmark drv);
            };

            html-parse = mkHaskell "html-parse" html-parse-src {
              adjustDrv = args: drv:
                let ghcv = args.ghcver or "ghc8104"; in
                if builtins.compareVersions pkgs.haskell.compiler."${ghcv}".version "9.0" < 0
                then haskellAdj drv
                else pkgs.haskell.lib.doJailbreak (haskellAdj drv);
            };
          });
      };
}
