# You can build this repository using Nix by running:
#
#     $ nix-build -A list-transformer release.nix
#
# You can also open up this repository inside of a Nix shell by running:
#
#     $ nix-shell -A list-transformer.env release.nix
#
# ... and then Nix will supply the correct Haskell development environment for
# you
let
  config = {
    packageOverrides = pkgs: {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: {
          list-transformer = haskellPackagesNew.callPackage ./default.nix { };
        };
      };
    };
  };

  pkgs =
    import <nixpkgs> { inherit config; };

in
  { list-transformer = pkgs.haskellPackages.list-transformer;
  }
