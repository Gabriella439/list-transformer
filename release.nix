let
  config = {
    packageOverrides = pkgs: {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = pkgs.haskell.lib.packageSourceOverrides {
          list-transformer = ./.;
        };
      };
    };
  };

  pkgs =
    import <nixpkgs> { inherit config; };

in
  { inherit (pkgs.haskellPackages) list-transformer;
  }
