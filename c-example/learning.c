/* SPDX-License-Identifier: GPL-3.0-or-later */
#include <inttypes.h>
#include <stdio.h>

#include "example.h"

void demo_learning(suz_language *language)
{
    char *error = NULL;
    char command[1024];

    suz_session *unknown = session_with_word("кызлар");
    int before = suz_suggest_segmentation(unknown, language, &error);
    printf("\nunknown word: %d splits suggested\n", before);

    uint64_t morph = suz_next_id(unknown) - 1;
    snprintf(command, sizeof command, "split-morph\t%" PRIu64 "\t3\t%" PRIu64, morph, morph + 100);
    if (suz_perform_learning(unknown, language, command, &error) != 0)
        fail("split", error);
    snprintf(command, sizeof command, "set-gloss\t%" PRIu64 "\t1\tgirl\t0", morph);
    if (suz_perform_learning(unknown, language, command, &error) != 0)
        fail("gloss", error);
    snprintf(command, sizeof command, "set-gloss\t%" PRIu64 "\t1\t\t1\tPL", morph + 100);
    if (suz_perform_learning(unknown, language, command, &error) != 0)
        fail("gloss", error);
    show("taught by a person", unknown);

    suz_session *again = session_with_word("кызлар");
    int splits_again = suz_suggest_segmentation(again, language, &error);
    int glosses_again = suz_suggest_glosses(again, language, &error);
    printf("same word later: %d splits, %d glosses\n", splits_again, glosses_again);
    show("suggested from learned weights", again);

    char *weights = suz_language_to_text(language);
    printf("-- language after learning\n%s", weights);
    suz_free(weights);

    suz_session_free(again);
    suz_session_free(unknown);
}
