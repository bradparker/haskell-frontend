with import <nixpkgs> {};
packages: config:
let
  source = fetchFromGitHub {
    owner = "dmjio";
    repo = "miso";
    rev = lib.fileContents ./miso-rev.txt;
    sha256 = lib.fileContents  ./miso-sha.txt;
  };
in
  packages.callPackage "${source}/miso-ghcjs.nix" config
