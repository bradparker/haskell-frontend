{ mkDerivation, base, ghcjs-dev-client, miso, miso-style, stdenv }:
mkDerivation {
  pname = "haskell-frontend";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base ghcjs-dev-client miso miso-style
  ];
  license = stdenv.lib.licenses.bsd3;
}
