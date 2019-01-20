let
  nixpkgs = import ./nixpkgs.nix;
  tools = [ nixpkgs.haskellPackages.ghcjs-dev-server ];
  package = nixpkgs.haskell.lib.addBuildDepends (import ./.) tools;
in
  package.env
