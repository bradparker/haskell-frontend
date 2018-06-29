{ nixpkgs ? import ./nix/nixpkgs.nix {} }:
let
  packages = nixpkgs.haskell.packages.ghcjs;

  miso = import ./nix/miso.nix packages {};
  miso-style = import ./nix/miso-style.nix packages { inherit miso; };
  ghcjs-dev-client = import ./nix/ghcjs-dev-server.nix "client" packages {};
in packages.callPackage ./package.nix {
  inherit miso;
  inherit miso-style;
  inherit ghcjs-dev-client;
}
