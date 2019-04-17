let
  pkgs = import ../../nix {};
in
  pkgs.ghc.callPackage ./metagen.nix { }
