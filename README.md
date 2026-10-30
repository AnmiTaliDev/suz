# Suz

Haskell library for morpheme-level interlinear glossing. The engine holds no language knowledge; language data is supplied as weight tables.

## Dependencies

- GHC 9.6 or newer
- cabal-install 3.10 or newer
- Libraries: `base`, `bytestring`, `containers`, `text`

## Build

```
cabal build
cabal test
cabal run suz-examples
```

The build is static. A GHC without static libraries (for example the Arch Linux `ghc` package) fails with `Could not find module 'Prelude'`; use a GHC installed with ghcup.

## C interface

The `suz` foreign library exports the functions declared in [include/suz.h](include/suz.h). Build it and the example:

```
cabal build flib:suz
make -C c-example
c-example/suz-c-example
```

`make` takes the library location from `SUZ_BUILD`, which defaults to the build directory of the `ghc` found in `PATH`.

## License

GNU General Public License, version 3 or later. See [LICENSE](LICENSE).
