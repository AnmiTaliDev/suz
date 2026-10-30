SUZ_BUILD ?= $(abspath ../dist-newstyle/build/x86_64-linux/ghc-9.6.7/suz-0.1.0.0/f/suz/build/suz)
CC ?= cc
CFLAGS ?= -Wall -Wextra -O2

SOURCES := $(wildcard *.c)
HEADERS := $(wildcard *.h) $(wildcard ../include/suz.h) $(wildcard ../include/suz/*.h)

suz-c-example: $(SOURCES) $(HEADERS)
	$(CC) $(CFLAGS) -I../include $(SOURCES) -L$(SUZ_BUILD) -lsuz -Wl,-rpath,$(SUZ_BUILD) -o $@

clean:
	rm -f suz-c-example

.PHONY: clean
