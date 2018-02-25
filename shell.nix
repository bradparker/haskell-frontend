with import <nixpkgs> {};
{ nixpkgs ? import (fetchgit {
    inherit (builtins.fromJSON (builtins.readFile ./nix-revisions/nixpkgs.json)) url rev sha256;
  }) {} }:
let
  haskellPackages = nixpkgs.haskellPackages;
  hlint = haskellPackages.hlint;
  cabal = haskellPackages.cabal-install;
  ghcjs-dev-server = haskellPackages.callPackage ((fetchgit {
    inherit (builtins.fromJSON (builtins.readFile ./nix-revisions/ghcjs-dev-server.json)) url rev sha256;
  }) + "/server/package.nix") {};

  env = (import ./default.nix { inherit nixpkgs; }).env;
in
  nixpkgs.lib.overrideDerivation env (drv: {
    nativeBuildInputs = drv.nativeBuildInputs ++ [ hlint cabal ghcjs-dev-server ];
})
