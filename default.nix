with import <nixpkgs> {};
{ nixpkgs ? import (fetchgit {
    inherit (builtins.fromJSON (builtins.readFile ./nix-revisions/nixpkgs.json)) url rev sha256;
  }) {} }:
let
  haskellPackages = pkgs.haskell.packages.ghcjs;
  miso = haskellPackages.callPackage ((fetchgit {
    inherit (builtins.fromJSON (builtins.readFile ./nix-revisions/miso.json)) url rev sha256;
  }) + "/miso-ghcjs.nix") {};
  miso-style = haskellPackages.callPackage ((fetchgit {
    inherit (builtins.fromJSON (builtins.readFile ./nix-revisions/miso-style.json)) url rev sha256;
  }) + "/package.nix") {
    miso = miso;
  };
  ghcjs-dev-client = haskellPackages.callPackage ((fetchgit {
    inherit (builtins.fromJSON (builtins.readFile ./nix-revisions/ghcjs-dev-server.json)) url rev sha256;
  }) + "/client/package.nix") {};
in haskellPackages.callPackage ./package.nix {
  miso = miso;
  miso-style = miso-style;
  ghcjs-dev-client = ghcjs-dev-client;
}
