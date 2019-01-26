let
  nixpkgs = import ./nixpkgs.nix;
  tools = [];
  package = nixpkgs.haskell.lib.addBuildDepends (import ./.) tools;
in
  package.env
