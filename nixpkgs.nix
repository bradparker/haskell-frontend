let
  overlay = self: super:
    {
      haskellPackages = super.haskellPackages.extend (hself: hsuper: {
        # Newer miso please
        miso = super.haskell.lib.addBuildDepends
          (super.haskell.lib.enableCabalFlag (hself.callHackage "miso" "0.21.2.0" {}) "jsaddle")
          [
            hself.file-embed
            hself.jsaddle
            hself.jsaddle-warp
            hself.wai
            hself.wai-app-static
            hself.warp
            hself.websockets
          ];

        jsaddle = super.haskell.lib.doJailbreak hsuper.jsaddle;

        jsaddle-warp =
          let
            jsaddle-source = super.fetchFromGitHub {
              owner = "ghcjs";
              repo = "jsaddle";
              rev = "76d969d62c0c125bf58927224cac0448b429cd38";
              sha256 = "1fcw40w1x07daxwh4sbnf542v03p4858v8wbinsjw6vdabnm7aad";
            };
            package = hself.callPackage ("${jsaddle-source}/jsaddle-warp") {};
          in
            super.haskell.lib.dontCheck (package.overrideAttrs (oldAttrs: {
              patchPhase = ''
                substituteInPlace jsaddle-warp.cabal --replace "aeson >=0.8.0.2 && <1.3" "aeson >=0.8.0.2 && <1.4";
              '';
            }));
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

            # Newer miso please
            miso = super.haskell.lib.enableCabalFlag (hself.callHackage "miso" "0.21.2.0" {}) "jsaddle";
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
