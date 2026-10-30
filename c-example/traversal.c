/* SPDX-License-Identifier: GPL-3.0-or-later */
#include <inttypes.h>
#include <stdio.h>

#include "example.h"

static void print_morph(suz_session *session, int s, int t, int m)
{
    char *form = suz_morph_form(session, s, t, m);
    uint64_t id;
    suz_morph_id(session, s, t, m, &id);
    printf("    morph %" PRIu64 " %s", id, form);
    suz_free(form);

    if (suz_morph_glossed(session, s, t, m) == 1) {
        char *lexical = suz_morph_lexical(session, s, t, m);
        printf(" = %s", lexical);
        suz_free(lexical);
        int tags = suz_morph_tag_count(session, s, t, m);
        for (int i = 0; i < tags; i++) {
            char *tag = suz_morph_tag(session, s, t, m, i);
            printf(".%s", tag);
            suz_free(tag);
        }
    }
    printf("\n");
}

void demo_traversal(suz_language *language)
{
    char *error = NULL;

    suz_session *session = suz_session_from_text("Мин китапларны укыдым\n", &error);
    if (!session)
        fail("import", error);
    if (suz_suggest_segmentation(session, language, &error) < 0)
        fail("segmentation", error);
    if (suz_suggest_glosses(session, language, &error) < 0)
        fail("glosses", error);

    printf("-- traversal\n");
    for (int s = 0; s < suz_segment_count(session); s++) {
        for (int t = 0; t < suz_token_count(session, s); t++) {
            char *surface = suz_token_surface(session, s, t);
            printf("  token %s\n", surface);
            suz_free(surface);
            for (int m = 0; m < suz_morph_count(session, s, t); m++)
                print_morph(session, s, t, m);
        }
    }
    printf("\n");

    suz_session_free(session);
}
