SUZ_BUILD ?= $(abspath ../dist-newstyle/build/x86_64-linux/ghc-9.6.7/suz-0.1.0.0/f/suz/build/suz)
CC ?= cc
CFLAGS ?= -Wall -Wextra -O2

suz-c-example: main.c ../include/suz.h
	$(CC) $(CFLAGS) -I../include main.c -L$(SUZ_BUILD) -lsuz -Wl,-rpath,$(SUZ_BUILD) -o $@

clean:
	rm -f suz-c-example

.PHONY: clean
