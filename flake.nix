# cabal repl doesn't work due to not linking to libflint. I don't know why it
# doesn't -- perhaps something missing in cabal2nix or haskell-flake. Building
# and running works fine.
#
# A crude fix is `LD_LIBRARY_PATH=/nix/store/...-flint-x.y.z/lib cabal repl`.
# Get the correct path from `echo $NIX_LDFLAGS`.

{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    fortran-src.url   = "github:camfort/fortran-src";
    fortran-src.flake = false;
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [ inputs.haskell-flake.flakeModule ];
      perSystem = { self', pkgs, config, ... }: {
        packages.default = self'.packages.ghc96-camfort;
        devShells.default = self'.devShells.ghc96;
        haskellProjects.ghc96 = {
          basePackages = pkgs.haskell.packages.ghc96;
          packages.fortran-src.source = inputs.fortran-src;
          settings = {
            sbv = {
              broken = false;
            };
          };
        };

        # use streamLayeredImage so as to not place the image in the Nix store
        packages.ghc96-camfort-image = pkgs.dockerTools.streamLayeredImage {
          name = "camfort";
          # equivalent to `git rev-parse HEAD`
          # only exists on clean working tree, else set to "dev"
          tag = self.rev or "dev";
          config = {
            Entrypoint = [ "${pkgs.lib.getExe self'.packages.ghc96-camfort}" ];
          };
          maxLayers = 120; # less than Docker max layers to allow extending
          # Note that Z3 is configured using the C bindings, which then access
          # the executable. So no need to add Z3 in ourselves, it's added by the
          # main camfort derivation.
        };
      };
    };
}
