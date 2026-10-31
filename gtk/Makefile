GHC_VERSION := $(shell ghc --numeric-version)
SUZ_BUILD ?= $(abspath ../dist-newstyle/build/$(shell uname -m)-linux/ghc-$(GHC_VERSION)/suz-0.2.0.0/f/suz/build/suz)
LIBSUZ = $(wildcard $(SUZ_BUILD)/libsuz.so.*)
SONAME = $(basename $(basename $(notdir $(LIBSUZ))))
CC ?= cc
CFLAGS ?= -Wall -Wextra -O2
VERSION := 0.1.0
APP_ID := dev.anmitali.Suz
PREFIX ?= /usr/local

SOURCES := $(wildcard *.c)
HEADERS := $(wildcard *.h) $(wildcard ../include/suz.h) $(wildcard ../include/suz/*.h)
GTK_FLAGS := $(shell pkg-config --cflags --libs gtk4 libadwaita-1)

ifeq ($(shell pkg-config --atleast-version=1.8 libadwaita-1 && echo yes),)
$(error libadwaita 1.8 or newer is required)
endif

suz-gtk: $(SOURCES) $(HEADERS)
	mkdir -p lib
	ln -sf $(LIBSUZ) lib/$(SONAME)
	$(CC) $(CFLAGS) -I../include -DSUZ_GTK_VERSION='"$(VERSION)"' \
	  -DSUZ_LANGUAGES_DIR='"$(abspath ../languages)"' -DSUZ_ICONS_DIR='"$(abspath ../data/icons)"' \
	  $(SOURCES) $(LIBSUZ) -Wl,-rpath,$(abspath lib) $(GTK_FLAGS) -o $@

install:
	$(CC) $(CFLAGS) -DSUZ_GTK_VERSION='"$(VERSION)"' $(SOURCES) \
	  $$(PKG_CONFIG_PATH=$(PREFIX)/lib/pkgconfig pkg-config --cflags --libs suz gtk4 libadwaita-1) -o suz-gtk-installed
	install -d $(DESTDIR)$(PREFIX)/bin $(DESTDIR)$(PREFIX)/share/applications $(DESTDIR)$(PREFIX)/share/metainfo
	install -m 755 suz-gtk-installed $(DESTDIR)$(PREFIX)/bin/suz-gtk
	install -m 644 ../data/$(APP_ID).desktop $(DESTDIR)$(PREFIX)/share/applications/
	install -m 644 ../data/$(APP_ID).metainfo.xml $(DESTDIR)$(PREFIX)/share/metainfo/
	install -d $(DESTDIR)$(PREFIX)/share/icons/hicolor/scalable/apps $(DESTDIR)$(PREFIX)/share/icons/hicolor/symbolic/apps
	install -m 644 ../data/icons/hicolor/scalable/apps/$(APP_ID).svg $(DESTDIR)$(PREFIX)/share/icons/hicolor/scalable/apps/
	install -m 644 ../data/icons/hicolor/symbolic/apps/$(APP_ID)-symbolic.svg $(DESTDIR)$(PREFIX)/share/icons/hicolor/symbolic/apps/
	rm -f suz-gtk-installed

uninstall:
	rm -f $(DESTDIR)$(PREFIX)/bin/suz-gtk
	rm -f $(DESTDIR)$(PREFIX)/share/applications/$(APP_ID).desktop
	rm -f $(DESTDIR)$(PREFIX)/share/metainfo/$(APP_ID).metainfo.xml
	rm -f $(DESTDIR)$(PREFIX)/share/icons/hicolor/scalable/apps/$(APP_ID).svg
	rm -f $(DESTDIR)$(PREFIX)/share/icons/hicolor/symbolic/apps/$(APP_ID)-symbolic.svg

clean:
	rm -rf suz-gtk suz-gtk-installed lib

.PHONY: install uninstall clean
