/* SPDX-License-Identifier: GPL-3.0-or-later */
#include <inttypes.h>
#include <stdio.h>

#include "example.h"

void demo_editing(suz_language *language)
{
    char *error = NULL;
    char command[1024];

    suz_session *session = suz_session_new();

    uint64_t n = suz_next_id(session);
    snprintf(command, sizeof command,
             "insert-segment\t0\t%" PRIu64 "\t3"
             "\t%" PRIu64 "\tМин\t1\t%" PRIu64 "\tмин\t0\t0"
             "\t%" PRIu64 "\tкитапларны\t1\t%" PRIu64 "\tкитапларны\t0\t0"
             "\t%" PRIu64 "\tукыдым\t1\t%" PRIu64 "\tукыдым\t0\t0",
             n, n + 1, n + 2, n + 3, n + 4, n + 5, n + 6);
    if (suz_perform(session, command, &error) != 0)
        fail("perform", error);

    int splits = suz_suggest_segmentation(session, language, &error);
    if (splits < 0)
        fail("segmentation", error);
    int glosses = suz_suggest_glosses(session, language, &error);
    if (glosses < 0)
        fail("glosses", error);
    printf("%d splits, %d glosses\n\n", splits, glosses);
    show("segmented and glossed", session);

    if (suz_undo(session, &error) != 0)
        fail("undo", error);
    show("after one undo", session);

    if (suz_redo(session, &error) != 0)
        fail("redo", error);

    char *snapshot = suz_snapshot(session);
    suz_session *restored = suz_session_from_snapshot(snapshot, &error);
    if (!restored)
        fail("snapshot", error);
    show("restored from snapshot", restored);

    char *journal = suz_journal(session);
    suz_session *replayed = suz_session_from_journal(journal, &error);
    if (!replayed)
        fail("journal", error);
    show("replayed from journal", replayed);

    if (suz_perform(session, "remove-token\t999999", &error) != 0) {
        printf("expected failure: %s\n", error);
        suz_free(error);
    }

    suz_session_free(replayed);
    suz_session_free(restored);
    suz_free(journal);
    suz_free(snapshot);
    suz_session_free(session);
}
