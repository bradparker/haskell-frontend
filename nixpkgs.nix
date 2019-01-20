let
  overlay = self: super:
    let
      ghcjs-dev-server-source = super.fetchFromGitHub {
        owner = "bradparker";
        repo = "ghcjs-dev-server";
        rev = "6a2d3e90f4d58e3eb60e9732031903de2a7f6aca";
        sha256 = "1h2nszsg5iyb31iha1zh8mbswhncdb4bhnma2p2m6hjxyva5sarg";
      };
    in
      {
        haskellPackages = super.haskellPackages.extend (hself: hsuper: {
          ghcjs-dev-server = hself.callPackage "${ghcjs-dev-server-source}/server/package.nix" {};
        });

        haskell = super.haskell // {
          packages = super.haskell.packages // {
            ghcjs = super.haskell.packages.ghcjs.extend (hself: hsuper: {
              # Doctest fails to build with a strange error.
              doctest = null;

              # These require doctest to run their tests.
              aeson = super.haskell.lib.dontCheck hsuper.aeson;
              http-types = super.haskell.lib.dontCheck hsuper.http-types;
              servant = super.haskell.lib.dontCheck hsuper.servant;

              # These have test suites which hang indefinitely.
              scientific = super.haskell.lib.dontCheck hsuper.scientific;
              tasty-quickcheck = super.haskell.lib.dontCheck hsuper.tasty-quickcheck;

              # Custom packages, not in hackage.
              ghcjs-dev-client = hself.callPackage "${ghcjs-dev-server-source}/client/package.nix" {};
            });
          };
        };
      };

  nixpkgs-source = builtins.fetchTarball {
    url = "https://releases.nixos.org/nixos/18.09/nixos-18.09.1922.97e0d53d669/nixexprs.tar.xz";
    sha256 = "0jl72zcsap4xjh483mjyvhmim45ghklw3pqr8mp0khwvh83422z6";
  };
in
  import nixpkgs-source {
    overlays = [overlay];
  }
