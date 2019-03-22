let
  overlay = self: super:
    let
      ghcjs-dev-server-source = super.fetchFromGitHub {
        owner = "bradparker";
        repo = "ghcjs-dev-server";
        rev = "a4a9286691f310c61d05aba72a54fbb06e2ac71c";
        sha256 = "1y318yf1nw2qn4dxgw4wivk4w83mmnm7ghw4whi25nky97m3zr6h";
      };
    in
      {
        haskellPackages = super.haskellPackages.extend (hself: hsuper: {
          ghcjs-dev-server = hself.callCabal2nix "ghcjs-dev-server" "${ghcjs-dev-server-source}/server" {};
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
              ghcjs-dev-client = hself.callCabal2nix "ghcjs-dev-client" "${ghcjs-dev-server-source}/client" {};
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
