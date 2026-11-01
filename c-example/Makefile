GHC_VERSION := $(shell ghc --numeric-version)
SUZ_BUILD ?= $(abspath ../dist-newstyle/build/$(shell uname -m)-linux/ghc-$(GHC_VERSION)/suz-0.4.0.0/f/suz/build/suz)
LIBSUZ = $(wildcard $(SUZ_BUILD)/libsuz.so.*)
SONAME = $(basename $(basename $(notdir $(LIBSUZ))))
CC ?= cc
CFLAGS ?= -Wall -Wextra -O2

SOURCES := $(wildcard *.c)
HEADERS := $(wildcard *.h) $(wildcard ../include/suz.h) $(wildcard ../include/suz/*.h)

suz-c-example: $(SOURCES) $(HEADERS)
	mkdir -p lib
	ln -sf $(LIBSUZ) lib/$(SONAME)
	$(CC) $(CFLAGS) -I../include $(SOURCES) $(LIBSUZ) -Wl,-rpath,$(abspath lib) -o $@

clean:
	rm -rf suz-c-example lib

.PHONY: clean
