/* SPDX-License-Identifier: GPL-3.0-or-later */
#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "suz.h"

static const char *tatar_weights =
    "language\tTatar\n"
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

static void fail(const char *what, char *error)
{
    fprintf(stderr, "%s: %s\n", what, error ? error : "failed");
    suz_free(error);
    suz_exit();
    exit(1);
}

static suz_session *session_with_word(const char *word)
{
    suz_session *session = suz_session_new();
    uint64_t n = suz_next_id(session);
    char command[256];
    char *error = NULL;
    snprintf(command, sizeof command,
             "insert-segment\t0\t%" PRIu64 "\t1\t%" PRIu64 "\t%s\t1\t%" PRIu64 "\t%s\t0",
             n, n + 1, word, n + 2, word);
    if (suz_perform(session, command, &error) != 0)
        fail("perform", error);
    return session;
}

static void show(const char *title, suz_session *session)
{
    char *text = suz_render(session);
    printf("-- %s\n%s\n", title, text);
    suz_free(text);
}

int main(void)
{
    char *error = NULL;

    suz_init();

    suz_language *tatar = suz_language_from_text(tatar_weights, &error);
    if (!tatar)
        fail("language", error);

    suz_session *session = suz_session_new();

    uint64_t n = suz_next_id(session);
    char command[1024];
    snprintf(command, sizeof command,
             "insert-segment\t0\t%" PRIu64 "\t3"
             "\t%" PRIu64 "\tМин\t1\t%" PRIu64 "\tмин\t0"
             "\t%" PRIu64 "\tкитапларны\t1\t%" PRIu64 "\tкитапларны\t0"
             "\t%" PRIu64 "\tукыдым\t1\t%" PRIu64 "\tукыдым\t0",
             n, n + 1, n + 2, n + 3, n + 4, n + 5, n + 6);
    if (suz_perform(session, command, &error) != 0)
        fail("perform", error);

    int splits = suz_suggest_segmentation(session, tatar, &error);
    if (splits < 0)
        fail("segmentation", error);
    int glosses = suz_suggest_glosses(session, tatar, &error);
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

    suz_session *unknown = session_with_word("кызлар");
    int before = suz_suggest_segmentation(unknown, tatar, &error);
    printf("\nunknown word: %d splits suggested\n", before);

    uint64_t morph = suz_next_id(unknown) - 1;
    snprintf(command, sizeof command, "split-morph\t%" PRIu64 "\t3\t%" PRIu64, morph, morph + 100);
    if (suz_perform_learning(unknown, tatar, command, &error) != 0)
        fail("split", error);
    snprintf(command, sizeof command, "set-gloss\t%" PRIu64 "\t1\tgirl\t0", morph);
    if (suz_perform_learning(unknown, tatar, command, &error) != 0)
        fail("gloss", error);
    snprintf(command, sizeof command, "set-gloss\t%" PRIu64 "\t1\t\t1\tPL", morph + 100);
    if (suz_perform_learning(unknown, tatar, command, &error) != 0)
        fail("gloss", error);
    show("taught by a person", unknown);

    suz_session *again = session_with_word("кызлар");
    int splits_again = suz_suggest_segmentation(again, tatar, &error);
    int glosses_again = suz_suggest_glosses(again, tatar, &error);
    printf("same word later: %d splits, %d glosses\n", splits_again, glosses_again);
    show("suggested from learned weights", again);

    char *weights = suz_language_to_text(tatar);
    printf("-- language after learning\n%s", weights);
    suz_free(weights);

    suz_session_free(again);
    suz_session_free(unknown);
    suz_free(snapshot);
    suz_free(journal);
    suz_session_free(replayed);
    suz_session_free(restored);
    suz_session_free(session);
    suz_language_free(tatar);
    suz_exit();
    return 0;
}
