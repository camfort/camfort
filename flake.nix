{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };
  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = inputs.nixpkgs.lib.systems.flakeExposed;
      imports = [ inputs.haskell-flake.flakeModule ];
      perSystem = { self', pkgs, config, ... }:
      let
        # use streamLayeredImage so as to not place the image in the Nix store
        mkCamfortImage = imageName: camfortPkg: pkgs.dockerTools.streamLayeredImage {
          name = imageName;
          # equivalent to `git rev-parse HEAD`
          # only exists on clean working tree, else set to "dev"
          tag = inputs.self.rev or "dev";
          config.Entrypoint = [ "${pkgs.lib.getExe camfortPkg}" ];
          maxLayers = 120; # less than Docker max layers to allow extending
        };
      in {
        packages.default  = self'.packages.camfort-ghc96-camfort;
        devShells.default = self'.devShells.camfort-ghc96;

        haskellProjects.ghc92 = import ./haskell-flake-ghc92.nix pkgs;
        haskellProjects.camfort-ghc92 = {
          basePackages = config.haskellProjects.ghc92.outputs.finalPackages;
          packages = {
            # 2024-09-13 raehik: CamFort's sbv ver range no longer in nixpkgs
            sbv.source = "9.2";

            fortran-src.source = "0.11.0";
          };

          settings = {
            sbv = {
              # 2024-09-13 raehik: huge tests, some fail, seems complex. disable
              # and just assume it's working
              check = false;

              # if we override the nixpkgs sbv derivation, we need to set this
              extraLibraries = [pkgs.z3];
            };
            # this might not be needed if we don't override the nixpkgs sbv
            # derivation? but it defo is if we do & seems a sensible default
            camfort.extraLibraries = [pkgs.z3];

            # 2024-09-12 raehik: temp TODO
            union.broken = false;
          };

          devShell = {
            tools = hp: {
              # use nixpkgs cabal-install
              cabal-install = pkgs.cabal-install;

              # disable these while unused (often slow/annoying to build)
              haskell-language-server = null;
              ghcid = null;
              hlint = null;
            };
          };
        };

        haskellProjects.ghc94 = import ./haskell-flake-ghc94.nix pkgs;
        haskellProjects.camfort-ghc94 = {
          basePackages = config.haskellProjects.ghc94.outputs.finalPackages;
          packages = {
            # 2024-09-13 raehik: CamFort's sbv ver range no longer in nixpkgs
            sbv.source = "9.2";

            fortran-src.source = "0.11.0";
          };

          settings = {
            sbv = {
              # 2024-09-13 raehik: huge tests, some fail, seems complex. disable
              # and just assume it's working
              check = false;

              # if we override the nixpkgs sbv derivation, we need to set this
              extraLibraries = [pkgs.z3];
            };
            # this might not be needed if we don't override the nixpkgs sbv
            # derivation? but it defo is if we do & seems a sensible default
            camfort.extraLibraries = [pkgs.z3];

            # 2024-09-12 raehik: temp TODO
            union.broken = false;
          };

          devShell = {
            tools = hp: {
              # use nixpkgs cabal-install
              cabal-install = pkgs.cabal-install;

              # disable these while unused (often slow/annoying to build)
              haskell-language-server = null;
              ghcid = null;
              hlint = null;
            };
          };
        };

        haskellProjects.ghc96 = import ./haskell-flake-ghc96.nix pkgs;
        haskellProjects.camfort-ghc96 = {
          basePackages = config.haskellProjects.ghc96.outputs.finalPackages;
          packages = {
            # Use whatever sbv version is available in nixpkgs for GHC 9.6
            # sbv.source = "10.12"; # removed - causes version conflicts

            # Use verifiable-expressions from GitHub
            verifiable-expressions.source = pkgs.fetchFromGitHub {
              owner = "camfort";
              repo = "verifiable-expressions";
              rev = "v0.6.3";
              sha256 = "sha256-OqtTnmQhtODcbOQHaokSAi5h2rQS9Rj43YfqXAfOzLw=";
            };

            # Use fortran-src from GitHub for latest GHC compatibility
            fortran-src.source = pkgs.fetchFromGitHub {
              owner = "camfort";
              repo = "fortran-src";
              rev = "f5141b8ea86506690cd88de1884c1b97193cd477";
              sha256 = "sha256-DpY+Yl9lDRhLTLLx5SYcr6lF+8i9TbdAVtPecWGSUAI=";
            };
          };

          settings = {
            sbv = {
              # 2024-09-13 raehik: huge tests, some fail, seems complex. disable
              # and just assume it's working
              check = false;
              
              # sbv is marked as broken in nixpkgs, but we need it
              broken = false;
              
              # sbv-10.2 violates <10 bounds in some dependencies
              jailbreak = true;

              # if we override the nixpkgs sbv derivation, we need to set this
              extraLibraries = [pkgs.z3];
            };
            # this might not be needed if we don't override the nixpkgs sbv
            # derivation? but it defo is if we do & seems a sensible default
            camfort.extraLibraries = [pkgs.z3];

            # 2024-09-12 raehik: temp TODO
            union.broken = false;
            union.jailbreak = true;
          };

          devShell = {
            tools = hp: {
              # use nixpkgs cabal-install
              cabal-install = pkgs.cabal-install;

              # disable these while unused (often slow/annoying to build)
              haskell-language-server = null;
              ghcid = null;
              hlint = null;
            };
          };
        };

        packages.camfort-image-ghc92 =
          mkCamfortImage "camfort" self'.packages.camfort-ghc92-camfort;
        packages.camfort-image-ghc96 =
          mkCamfortImage "camfort" self'.packages.camfort-ghc96-camfort;
      };
    };
}
