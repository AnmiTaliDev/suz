/* SPDX-License-Identifier: GPL-3.0-or-later */
#include <stdio.h>

#include "example.h"

static void print_candidate(suz_language *language, const char *form, int i)
{
    double weight = 0;
    suz_language_candidate_weight(language, form, i, &weight);
    char *lexical = suz_language_candidate_lexical(language, form, i);
    printf("  %s %g %s", form, weight, lexical);
    suz_free(lexical);

    int tags = suz_language_candidate_tag_count(language, form, i);
    for (int t = 0; t < tags; t++) {
        char *tag = suz_language_candidate_tag(language, form, i, t);
        printf(".%s", tag);
        suz_free(tag);
    }
    printf("\n");
}

void demo_language(suz_language *language)
{
    char *name = suz_language_name(language);
    printf("== language %s\n", name);
    suz_free(name);

    printf("tags:");
    int tags = suz_language_tag_count(language);
    for (int i = 0; i < tags; i++) {
        char *tag = suz_language_tag(language, i);
        printf(" %s", tag);
        suz_free(tag);
    }
    printf("\n");

    printf("candidates:\n");
    int candidates = suz_language_candidate_count(language, "лар");
    for (int i = 0; i < candidates; i++)
        print_candidate(language, "лар", i);
}
