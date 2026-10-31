/* SPDX-License-Identifier: GPL-3.0-or-later */
#include "example.h"

static const char *tatar_weights =
    "language\tTatar\n"
    "punctuation\t.,!?\n"
    "foldcase\tturkic\n"
    "tag\tPL\n"
    "tag\tACC\n"
    "tag\tPST\n"
    "tag\t1SG\n"
    "weight\tмин\t10\t-\t1SG\n"
    "weight\tкитап\t10\tbook\n"
    "weight\tукы\t10\tread\n"
    "weight\tлар\t8\t-\tPL\n"
    "weight\tны\t9\t-\tACC\n"
    "weight\tды\t9\t-\tPST\n"
    "weight\tм\t9\t-\t1SG\n";

int main(void)
{
    char *error = NULL;

    suz_init();

    suz_language *tatar = suz_language_from_text(tatar_weights, &error);
    if (!tatar)
        fail("language", error);

    demo_editing(tatar);
    demo_learning(tatar);
    demo_exchange(tatar);
    demo_traversal(tatar);
    demo_language(tatar);
    demo_builtin();

    suz_language_free(tatar);
    suz_exit();
    return 0;
}
