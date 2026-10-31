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

void demo_builtin(void)
{
    printf("== bundled languages\n");
    int count = suz_builtin_language_count();
    for (int i = 0; i < count; i++) {
        char *name = suz_builtin_language_name(i);
        printf("  %s\n", name);
        suz_free(name);
    }

    char *error = NULL;
    suz_language *kazakh = suz_language_builtin("Kazakh", &error);
    if (!kazakh)
        fail("builtin", error);
    suz_session *session = suz_session_from_text_with_language(kazakh, "Мен кітаптарды оқыдым.", &error);
    if (!session)
        fail("import", error);
    if (suz_suggest_segmentation(session, kazakh, &error) < 0)
        fail("segmentation", error);
    if (suz_suggest_glosses(session, kazakh, &error) < 0)
        fail("glosses", error);
    show("annotated with the bundled Kazakh", session);
    suz_session_free(session);
    suz_language_free(kazakh);

    suz_language *missing = suz_language_builtin("Klingon", &error);
    if (missing)
        fail("builtin", "unexpected language");
    printf("expected failure: %s\n", error);
    suz_free(error);
}
