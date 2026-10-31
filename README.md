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
sh scripts/bundle.sh path/to/libsuz.so.X.Y.Z destination-directory
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

The `languages` directory holds a weight table per language in the language text format. The files are data, not code, and do not refer to each other. Each covers a handful of stems and endings and is a starting point, not a description of the language.

`cabal install` puts the files in the package data directory and `make install` puts them in `PREFIX/share/suz/languages`.

From C, read a file and pass its text to `suz_language_from_text()`. From Haskell, pass it to `parseLanguage` and find the files with `getDataFileName` from the generated `Paths_suz` module.

## GTK application

The `gtk` directory holds a GNOME application. It opens a plain text file, splits the words into morphs and glosses them with a language from `languages/`, and lets you correct the glosses. Documents are saved as snapshots and can be exported as interlinear text, Toolbox, CoNLL-U and JSON. It needs GTK 4.12 and libadwaita 1.8 or newer.

```
cabal build flib:suz
make -C gtk
gtk/suz-gtk text.txt
```

The `gtk/samples` directory has texts in Kazakh and Turkish and an annotated Kazakh document to try it on. The texts contain words the language does not know and one wrongly split word, to practise corrections.

To install it, install the C library first, then the application:

```
make install PREFIX=/usr/local
make -C gtk install PREFIX=/usr/local
```

Languages are read from `SUZ_LANGUAGES_DIR` (a list of directories), from `suz/languages` in the user data directory and from `suz/languages` in the system data directories. The weights learned from corrections are not written back.

## License

GNU General Public License, version 3 or later. See [LICENSE](LICENSE).
