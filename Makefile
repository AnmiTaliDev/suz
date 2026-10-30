GHC_VERSION := $(shell ghc --numeric-version)
SUZ_BUILD ?= $(abspath dist-newstyle/build/$(shell uname -m)-linux/ghc-$(GHC_VERSION)/suz-0.1.0.0/f/suz/build/suz)
VERSION := 0.1.0.0
PREFIX ?= /usr/local
LIBDIR = $(PREFIX)/lib/suz

all:
	cabal build flib:suz

install: all
	install -d $(DESTDIR)$(PREFIX)/include/suz $(DESTDIR)$(PREFIX)/lib/pkgconfig
	install -m 644 include/suz.h $(DESTDIR)$(PREFIX)/include/
	install -m 644 include/suz/*.h $(DESTDIR)$(PREFIX)/include/suz/
	scripts/bundle.sh $(SUZ_BUILD)/libsuz.so $(DESTDIR)$(LIBDIR)
	printf '%s\n' \
	  'Name: suz' \
	  'Description: Morpheme-level interlinear glossing engine' \
	  'Version: $(VERSION)' \
	  'Cflags: -I$(PREFIX)/include' \
	  'Libs: -L$(LIBDIR) -lsuz -Wl,-rpath,$(LIBDIR)' \
	  > $(DESTDIR)$(PREFIX)/lib/pkgconfig/suz.pc

uninstall:
	rm -rf $(DESTDIR)$(PREFIX)/include/suz $(DESTDIR)$(LIBDIR)
	rm -f $(DESTDIR)$(PREFIX)/include/suz.h $(DESTDIR)$(PREFIX)/lib/pkgconfig/suz.pc

.PHONY: all install uninstall
