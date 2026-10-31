GHC_VERSION := $(shell ghc --numeric-version)
SUZ_BUILD ?= $(abspath dist-newstyle/build/$(shell uname -m)-linux/ghc-$(GHC_VERSION)/suz-0.1.0.0/f/suz/build/suz)
VERSION := 0.1.0.0
ABI_VERSION := 0.0.0
PREFIX ?= /usr/local
LIBDIR = $(PREFIX)/lib/suz

all:
	cabal build flib:suz

install: all
	install -d $(DESTDIR)$(PREFIX)/include/suz $(DESTDIR)$(PREFIX)/lib/pkgconfig
	install -m 644 include/suz.h $(DESTDIR)$(PREFIX)/include/
	install -m 644 include/suz/*.h $(DESTDIR)$(PREFIX)/include/suz/
	install -d $(DESTDIR)$(PREFIX)/share/suz/languages
	install -m 644 languages/*.language $(DESTDIR)$(PREFIX)/share/suz/languages/
	sh scripts/bundle.sh $(SUZ_BUILD)/libsuz.so.$(ABI_VERSION) $(DESTDIR)$(LIBDIR)
	printf '%s\n' \
	  'Name: suz' \
	  'Description: Morpheme-level interlinear glossing engine' \
	  'Version: $(VERSION)' \
	  'Cflags: -I$(PREFIX)/include' \
	  'Libs: -L$(LIBDIR) -lsuz -Wl,-rpath,$(LIBDIR)' \
	  > $(DESTDIR)$(PREFIX)/lib/pkgconfig/suz.pc

uninstall:
	rm -rf $(DESTDIR)$(PREFIX)/include/suz $(DESTDIR)$(LIBDIR) $(DESTDIR)$(PREFIX)/share/suz
	rm -f $(DESTDIR)$(PREFIX)/include/suz.h $(DESTDIR)$(PREFIX)/lib/pkgconfig/suz.pc

.PHONY: all install uninstall
