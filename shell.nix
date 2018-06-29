{ nixpkgs ? import ./nix/nixpkgs.nix {} }:
let
  packages = nixpkgs.haskell.packages.ghc802;

  cabal = packages.cabal-install;
  ghcjs-dev-server = import ./nix/ghcjs-dev-server.nix "server" packages {};
  hlint = packages.hlint;

  env = (import ./default.nix { inherit nixpkgs; }).env;
in
  nixpkgs.lib.overrideDerivation env (drv: {
    nativeBuildInputs = drv.nativeBuildInputs ++ [ hlint cabal ghcjs-dev-server ];
  })
