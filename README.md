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

The build is static. A GHC without static libraries (for example the Arch Linux `ghc` package) fails with `Could not find module 'Prelude'`; use a GHC installed with ghcup, or build dynamically:

```
cabal test --enable-shared --disable-library-vanilla --ghc-options=-dynamic
```

For the C library on such a GHC, build `flib:suz` with the same options and run `make -o all install SUZ_BUILD=path/to/build/suz`, where the path is the directory that holds `libsuz.so.X.Y.Z`.

## C interface

The `suz` foreign library exports the functions declared in [include/suz.h](include/suz.h). Build it and the example:

```
cabal build flib:suz
make -C c-example
c-example/suz-c-example
```

`make` takes the library location from `SUZ_BUILD`, which defaults to the build directory of the `ghc` found in `PATH`.

`libsuz.so` needs the Haskell runtime libraries of the GHC that built it. To collect them in one directory that can be moved to another machine with the same architecture, run:

```
scripts/bundle.sh path/to/libsuz.so.X.Y.Z destination-directory
```

The library looks for its dependencies next to itself. The C library, the math library and GMP are taken from the system.

To install the headers, the library with its runtime and a `suz.pc` file:

```
make install PREFIX=/usr/local
```

The library goes to `PREFIX/lib/suz`, the headers to `PREFIX/include`. `DESTDIR` is supported. `make uninstall` removes the installed files. A program is built with:

```
cc main.c $(pkg-config --cflags --libs suz)
```

The shared object is versioned with `lib-version-info` in `suz.cabal` (`current:revision:age`, soname `libsuz.so.current-age`). Raise `current` and `age` when functions are added, reset `age` to 0 when an existing function changes or is removed. `ABI_VERSION` in `Makefile` must match.

`pkg-config` finds `suz.pc` through `PKG_CONFIG_PATH=PREFIX/lib/pkgconfig` when the prefix is not a system one.

## Languages

The library ships small weight tables, each in its own module under `Suz.Languages`: Azerbaijani, CrimeanTatar, English, French, German, Italian, Kazakh, Kyrgyz, Latin, Polish, Russian, Spanish, Tatar, Turkish, Turkmen and Uzbek. Each covers a handful of stems and endings. They serve as examples and as a starting point for a real table, not as a description of the language.

```haskell
import Suz.Languages.Turkish (turkish)
```

From C, `suz_builtin_language_count()` and `suz_builtin_language_name()` list the bundled languages and `suz_language_builtin()` returns a copy of one by name.

## License

GNU General Public License, version 3 or later. See [LICENSE](LICENSE).
