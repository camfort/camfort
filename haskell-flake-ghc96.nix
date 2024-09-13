pkgs: {
  # disable local project options (always do this for package sets)
  defaults.packages = {};
  devShell.enable = false;
  autoWire = [];

  basePackages = pkgs.haskell.packages.ghc96;
  packages = {
    # GHC 9.6 libraries
    singletons-th.source   = "3.2";
    singletons-base.source = "3.2";
    #singletons.source = "3.0.1";
    th-desugar.source = "1.15";
    #th-abstraction.source = "0.4.5.0";
  };
}
