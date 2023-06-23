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
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [ inputs.haskell-flake.flakeModule ];
      perSystem = { self', pkgs, config, ... }: {
        packages.default = self'.packages.camfort;
        #haskellProjects.ghc96 = import ./haskell-flake-ghc96.nix pkgs;
        haskellProjects.default = {
          #basePackages = config.haskellProjects.ghc96.outputs.finalPackages;
          packages = {
            fortran-src.source = "0.11.0";
          };

          settings = {
            sbv = {
              # 2023-04-18 raehik: sbv-9.0 broken; seems tests fail. try ignoring
              check = false;
              broken = false;
            };
          };

          devShell = {
            # libflint doesn't get configured properly for `cabal repl` -- fix
            # it here
            # curiously, `cabal build` works... maybe $NIX_LDFLAGS etc. get used
            # by the GHC invocation involved there.
            # I don't have to do this for pkg-config libraries (of which
            # libflint is not one). interesting!
            mkShellArgs.shellHook = ''
              export LD_LIBRARY_PATH='${pkgs.flint}/lib'
            '';

            tools = hp: {
              ghcid = null; # broken on GHC 9.6? old fsnotify
              hlint = null; # broken on GHC 9.6? old
              haskell-language-server = null; # TAKES AGES TO BUILD FFS
            };
          };
        };
      };
    };
}
