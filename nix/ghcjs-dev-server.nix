with import <nixpkgs> {};
project: packages: config:
let
  source = fetchFromGitHub {
    owner = "bradparker";
    repo = "ghcjs-dev-server";
    rev = lib.fileContents ./ghcjs-dev-server-rev.txt;
    sha256 = lib.fileContents  ./ghcjs-dev-server-sha.txt;
  };
in
  haskell.lib.doJailbreak (packages.callPackage "${source}/${project}/package.nix" config)
