GHC_VERSION := $(shell ghc --numeric-version)
SUZ_BUILD ?= $(abspath ../dist-newstyle/build/$(shell uname -m)-linux/ghc-$(GHC_VERSION)/suz-0.4.0.0/f/suz/build/suz)
LIBSUZ = $(wildcard $(SUZ_BUILD)/libsuz.so.*)
SONAME = $(basename $(basename $(notdir $(LIBSUZ))))
CC ?= cc
CFLAGS ?= -Wall -Wextra -O2
VERSION := 0.4.0.0
APP_ID := dev.anmitali.Suz
PREFIX ?= /usr/local
GETTEXT_PACKAGE := suz
LINGUAS := $(shell cat po/LINGUAS)
MOS := $(foreach lang,$(LINGUAS),locale/$(lang)/LC_MESSAGES/$(GETTEXT_PACKAGE).mo)

SOURCES := $(wildcard *.c)
HEADERS := $(wildcard *.h) $(wildcard ../include/suz.h) $(wildcard ../include/suz/*.h)
GTK_FLAGS := $(shell pkg-config --cflags --libs gtk4 libadwaita-1)

ifeq ($(shell pkg-config --atleast-version=1.8 libadwaita-1 && echo yes),)
$(error libadwaita 1.8 or newer is required)
endif

suz-gtk: $(SOURCES) $(HEADERS) $(MOS)
	mkdir -p lib
	ln -sf $(LIBSUZ) lib/$(SONAME)
	$(CC) $(CFLAGS) -I../include -DSUZ_GTK_VERSION='"$(VERSION)"' \
	  -DSUZ_LANGUAGES_DIR='"$(abspath ../languages)"' -DSUZ_ICONS_DIR='"$(abspath ../data/icons)"' \
	  -DGETTEXT_PACKAGE='"$(GETTEXT_PACKAGE)"' -DLOCALEDIR='"$(abspath locale)"' \
	  $(SOURCES) $(LIBSUZ) -Wl,-rpath,$(abspath lib) $(GTK_FLAGS) -o $@

locale/%/LC_MESSAGES/$(GETTEXT_PACKAGE).mo: po/%.po
	mkdir -p $(dir $@)
	msgfmt --check -o $@ $<

po/$(GETTEXT_PACKAGE).pot: $(SOURCES) ../data/$(APP_ID).desktop ../data/$(APP_ID).metainfo.xml po/POTFILES.in
	xgettext --from-code=UTF-8 --add-comments --keyword=_ --keyword=N_ --package-name=Suz \
	  --package-version=$(VERSION) --files-from=po/POTFILES.in --output=$@

pot: po/$(GETTEXT_PACKAGE).pot

update-po: po/$(GETTEXT_PACKAGE).pot
	for lang in $(LINGUAS); do msgmerge --update --backup=none --quiet po/$$lang.po $<; done

install:
	$(CC) $(CFLAGS) -DSUZ_GTK_VERSION='"$(VERSION)"' \
	  -DGETTEXT_PACKAGE='"$(GETTEXT_PACKAGE)"' -DLOCALEDIR='"$(PREFIX)/share/locale"' $(SOURCES) \
	  $$(PKG_CONFIG_PATH=$(PREFIX)/lib/pkgconfig pkg-config --cflags --libs suz gtk4 libadwaita-1) -o suz-gtk-installed
	install -d $(DESTDIR)$(PREFIX)/bin $(DESTDIR)$(PREFIX)/share/applications $(DESTDIR)$(PREFIX)/share/metainfo $(DESTDIR)$(PREFIX)/share/mime/packages
	install -m 755 suz-gtk-installed $(DESTDIR)$(PREFIX)/bin/suz-gtk
	msgfmt --desktop --template=../data/$(APP_ID).desktop -d po -o $(DESTDIR)$(PREFIX)/share/applications/$(APP_ID).desktop
	msgfmt --xml --template=../data/$(APP_ID).metainfo.xml -d po -o $(DESTDIR)$(PREFIX)/share/metainfo/$(APP_ID).metainfo.xml
	install -m 644 ../data/$(APP_ID).xml $(DESTDIR)$(PREFIX)/share/mime/packages/$(APP_ID).xml
	test -n "$(DESTDIR)" || update-mime-database $(PREFIX)/share/mime
	for lang in $(LINGUAS); do \
	  install -d $(DESTDIR)$(PREFIX)/share/locale/$$lang/LC_MESSAGES; \
	  msgfmt --check -o $(DESTDIR)$(PREFIX)/share/locale/$$lang/LC_MESSAGES/$(GETTEXT_PACKAGE).mo po/$$lang.po; \
	done
	install -d $(DESTDIR)$(PREFIX)/share/icons/hicolor/scalable/apps $(DESTDIR)$(PREFIX)/share/icons/hicolor/symbolic/apps
	install -m 644 ../data/icons/hicolor/scalable/apps/$(APP_ID).svg $(DESTDIR)$(PREFIX)/share/icons/hicolor/scalable/apps/
	install -m 644 ../data/icons/hicolor/symbolic/apps/$(APP_ID)-symbolic.svg $(DESTDIR)$(PREFIX)/share/icons/hicolor/symbolic/apps/
	rm -f suz-gtk-installed

uninstall:
	rm -f $(DESTDIR)$(PREFIX)/bin/suz-gtk
	rm -f $(DESTDIR)$(PREFIX)/share/applications/$(APP_ID).desktop
	rm -f $(DESTDIR)$(PREFIX)/share/metainfo/$(APP_ID).metainfo.xml
	rm -f $(DESTDIR)$(PREFIX)/share/mime/packages/$(APP_ID).xml
	test -n "$(DESTDIR)" || update-mime-database $(PREFIX)/share/mime
	rm -f $(DESTDIR)$(PREFIX)/share/icons/hicolor/scalable/apps/$(APP_ID).svg
	rm -f $(DESTDIR)$(PREFIX)/share/icons/hicolor/symbolic/apps/$(APP_ID)-symbolic.svg
	for lang in $(LINGUAS); do rm -f $(DESTDIR)$(PREFIX)/share/locale/$$lang/LC_MESSAGES/$(GETTEXT_PACKAGE).mo; done

clean:
	rm -rf suz-gtk suz-gtk-installed lib locale

.PHONY: pot update-po install uninstall clean
