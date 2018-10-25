let
  pkgs = import <nixpkgs> { };

in
  { husky = pkgs.haskellPackages.callPackage ./default.nix { };
  }
