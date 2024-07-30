{
  # $ nix develop
  # $ nix build
  # $ nix develop .#kvitable.ghc884.env
  # $ nix build  .#kvitable.ghc884.default

  description = "Haskell Key/Value Indexed Table library";

  nixConfig.bash-prompt-suffix = "kvitable} ";

  inputs = {
    nixpkgs8.url = github:nixos/nixpkgs/23.05;
    nixpkgs.url = github:nixos/nixpkgs/nixpkgs-unstable;
    levers = {
      type = "github";
      owner = "kquick";
      repo = "nix-levers";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    html-parse-src = { flake = false; url = github:bgamari/html-parse; };
    named-text = {
      url = "github:kquick/named-text";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.levers.follows = "levers";
      inputs.sayable.follows = "sayable";
      inputs.parameterized-utils-src.follows = "parameterized-utils-src";
      # inputs.tasty-checklist.follows = "tasty-checklist";
      # inputs.hedgehog-src.follows = "hedgehog-src";
      # inputs.hedgehog-classes-src.follows = "hedgehog-classes-src";
      # inputs.tasty-hedgehog-src.follows = "tasty-hedgehog-src";
    };
    named-text8 = {
      url = "github:kquick/named-text";
      inputs.nixpkgs.follows = "nixpkgs8";
      inputs.levers.follows = "levers";
      inputs.sayable.follows = "sayable8";
      inputs.parameterized-utils-src.follows = "parameterized-utils-src";
      # inputs.tasty-checklist.follows = "tasty-checklist";
      # inputs.hedgehog-src.follows = "hedgehog-src";
      # inputs.hedgehog-classes-src.follows = "hedgehog-classes-src";
      # inputs.tasty-hedgehog-src.follows = "tasty-hedgehog-src";
    };
    parameterized-utils-src = {
      url = "github:GaloisInc/parameterized-utils";
      flake = false;
    };
    sayable = {
      url = "github:kquick/sayable/9c76cc5";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.levers.follows = "levers";
    };
    sayable8 = {
      url = "github:kquick/sayable/9c76cc5";
      inputs.nixpkgs.follows = "nixpkgs8";
      inputs.levers.follows = "levers";
    };
  };

  outputs = { self, nixpkgs, nixpkgs8, levers
            , html-parse-src, named-text, named-text8, sayable, sayable8
            , parameterized-utils-src
            }: rec
      {
        devShells = levers.eachSystem (s:
          let kpkg = packages.${s}.default.env.overrideAttrs (old:
                {
                  nativeBuildInputs = let pkgs = import nixpkgs { system=s; }; in [
                pkgs.haskell.compiler.ghc98
                pkgs.cabal-install
                  ] ++ old.nativeBuildInputs;
                }
              );
          in { kvitable = kpkg; default = kpkg; });

        packages = levers.eachSystem (system:
          let mkHaskell9 = levers.mkHaskellPkg {
                inherit nixpkgs system;
              };
              mkHaskell8 = levers.mkHaskellPkg {
                inherit system;
                nixpkgs = nixpkgs8;
              };
              mkHaskell = name: src: ovrDrvOrArgs:
                let blds8 = mkHaskell8 name src ovrDrvOrArgs;
                    blds9 = mkHaskell9 name src ovrDrvOrArgs;
                in (blds8 // blds9);
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
              inherit html-parse named-text sayable;
              adjustDrv = args: drv:
                let ghcv = args.ghcver or "ghc8104";
                    drv2 = haskellAdj drv;
                in if builtins.compareVersions pkgs.haskell.compiler."${ghcv}".version "9.0" < 0
                   then drv2.overrideAttrs (oldAttrs: {
                     named-text = named-text8;
                   })
                   else drv2;
            };
            kvitable-test = mkHaskell "kvitable-test" self {
              inherit html-parse named-text sayable;
              adjustDrv = args: drv:
                let ghcv = args.ghcver or "ghc8104";
                    drv2 = pkgs.haskell.lib.doCheck (haskellAdj drv);
                in if builtins.compareVersions pkgs.haskell.compiler."${ghcv}".version "9.0" < 0
                   then drv2.overrideAttrs (oldAttrs: {
                     named-text = named-text8;
                   })
                   else drv2;
            };
            kvitable-doc = mkHaskell "kvitable-doc" self {
              inherit html-parse named-text sayable;
              adjustDrv = args: drv:
                let ghcv = args.ghcver or "ghc8104";
                    drv2 = with pkgs.haskell.lib; dontCheck (dontBenchmark drv);
                in if builtins.compareVersions pkgs.haskell.compiler."${ghcv}".version "9.0" < 0
                   then drv2.overrideAttrs (oldAttrs: {
                     named-text = named-text8;
                   })
                   else drv2;
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
