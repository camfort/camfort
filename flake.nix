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

      perSystem = { self', pkgs, ... }: {

        # Typically, you just want a single project named "default". But
        # multiple projects are also possible, each using different GHC version.
        haskellProjects.default = {
          # If you have a .cabal file in the root, this option is determined
          # automatically. Otherwise, specify all your local packages here.
          # packages.example.root = ./.;

          # The base package set representing a specific GHC version.
          # By default, this is pkgs.haskellPackages.
          # You may also create your own. See https://haskell.flake.page/package-set
          # basePackages = pkgs.haskellPackages;

          # Dependency overrides go here. See https://haskell.flake.page/dependency
          # source-overrides = { };
          overrides = self: super: with pkgs.haskell.lib; {
            # 2023-04-18 raehik: sbv-9.0 broken; seems tests fail. try ignoring
            sbv = dontCheck (unmarkBroken super.sbv);
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

          #  # Enabled by default
          #  enable = true;  
          #
          #  # Programs you want to make available in the shell.
          #  # Default programs can be disabled by setting to 'null'
          #  tools = hp: { fourmolu = hp.fourmolu; ghcid = null; };
          #
          #  hlsCheck.enable = true;
          };
        };

        # haskell-flake doesn't set the default package, but you can do it here.
        packages.default = self'.packages.camfort;
      };
    };
}
