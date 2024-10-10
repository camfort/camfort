pkgs: {
  # disable local project options (always do this for package sets)
  defaults.packages = {};
  devShell.enable = false;
  autoWire = [];

  basePackages = pkgs.haskell.packages.ghc92;
  packages = {
    # GHC 9.2 libraries
    singletons-th.source   = "3.1";
    singletons-base.source = "3.1";
    singletons.source = "3.0.1"; # req because singletons-th-3.1 had bad bounds
    th-desugar.source = "1.13.1";
    th-abstraction.source = "0.4.5.0";
  };

  # (note this is actually unused/we have to duplicate because it doesn't get
  # packed into basePackages or any key we can use... but nice to document here)
  devShell = {
    tools = hp: {
      # by default, haskell-flake uses the Haskell packages versions of these
      # tools (from hp). be warned, these can be a pain to build alternatively,
      # you may use nixpkgs versions via pkgs

      # as of 2024-09-13 nixpkgs-unstable can't build cabal-install on GHC 9.2.
      # no problem, use nixpkgs version (shouldn't really matter how built)
      cabal-install = pkgs.cabal-install;
    };
  };
}
