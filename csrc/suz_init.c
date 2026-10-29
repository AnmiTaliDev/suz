/* SPDX-License-Identifier: GPL-3.0-or-later */
#include "HsFFI.h"
#include "suz.h"

int suz_init(void)
{
    static char name[] = "suz";
    char *argv[] = {name, NULL};
    char **argv_ptr = argv;
    int argc = 1;
    hs_init(&argc, &argv_ptr);
    return 0;
}

void suz_exit(void)
{
    hs_exit();
}
