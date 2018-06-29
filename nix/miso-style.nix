with import <nixpkgs> {};
packages: config:
let
  source = fetchFromGitHub {
    owner = "bradparker";
    repo = "miso-style";
    rev = lib.fileContents ./miso-style-rev.txt;
    sha256 = lib.fileContents  ./miso-style-sha.txt;
  };
in
  packages.callPackage "${source}/package.nix" config
