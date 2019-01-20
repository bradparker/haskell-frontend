let
  nixpkgs = import ./nixpkgs.nix;
in
  nixpkgs.haskell.packages.ghcjs.callCabal2nix "haskell-frontend" ./. {}
