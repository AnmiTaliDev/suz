/* SPDX-License-Identifier: GPL-3.0-or-later */
#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>

#include "example.h"

void fail(const char *what, char *error)
{
    fprintf(stderr, "%s: %s\n", what, error ? error : "failed");
    suz_free(error);
    suz_exit();
    exit(1);
}

void show(const char *title, suz_session *session)
{
    char *text = suz_render(session);
    printf("-- %s\n%s\n", title, text);
    suz_free(text);
}

suz_session *session_with_word(const char *word)
{
    suz_session *session = suz_session_new();
    uint64_t n = suz_next_id(session);
    char command[256];
    char *error = NULL;
    snprintf(command, sizeof command,
             "insert-segment\t0\t%" PRIu64 "\t1\t%" PRIu64 "\t%s\t1\t%" PRIu64 "\t%s\t0\t0",
             n, n + 1, word, n + 2, word);
    if (suz_perform(session, command, &error) != 0)
        fail("perform", error);
    return session;
}
