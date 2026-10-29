# Suz

Haskell library for morpheme-level interlinear glossing. The engine holds no language knowledge; language data is supplied as weight tables.

## Dependencies

- GHC 9.6 or newer
- cabal-install 3.10 or newer
- Libraries: `base`, `containers`, `text`

## Build

```
cabal build
cabal test
cabal run suz-examples
```

If GHC is installed without static libraries (for example the Arch Linux package), the build fails with `Could not find module 'Prelude'`. Pass the dynamic linking flags to every cabal command:

```
cabal build --enable-shared --disable-library-vanilla --enable-executable-dynamic
cabal test --enable-shared --disable-library-vanilla --enable-executable-dynamic
cabal run suz-examples --enable-shared --disable-library-vanilla --enable-executable-dynamic
```

## C interface

The `suz` foreign library exports the functions declared in [include/suz.h](include/suz.h). Build it and the example:

```
cabal build flib:suz
make -C c-example
c-example/suz-c-example
```

Add the dynamic linking flags from the section above to the `cabal build` command when they are needed. `make` takes the library location from `SUZ_BUILD`, which defaults to the path of a local build on x86_64 Linux.

## License

GNU General Public License, version 3 or later. See [LICENSE](LICENSE).
