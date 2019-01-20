# Haskell frontend example

**N.B.** this is an experiment.

Putting together some tools and libraries into an example.

## Development

### System requirements

* Nix

### Running

```
$ nix-shell
$ ghcjs-dev-server -s dev -s src
```

The example will be available at http://localhost:8080.

As you make changes to Haskell source files in `./src` your browser will print compilation results / errors to the console and reload on completed compilations.

### Building

```
$ nix-build
```

You'll now have an `index.html` along with a bunch of JS files in `./result/bin/haskell-frontend.jsexe/`. Enjoy.
