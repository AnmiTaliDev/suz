/* SPDX-License-Identifier: GPL-3.0-or-later */
#include <stdio.h>

#include "example.h"

void demo_exchange(suz_language *language)
{
    char *error = NULL;

    suz_session *raw = suz_session_from_text("Мин китапларны укыдым.\nМин укыдым\n", &error);
    if (!raw)
        fail("import", error);
    if (suz_suggest_segmentation(raw, language, &error) < 0)
        fail("segmentation", error);
    if (suz_suggest_glosses(raw, language, &error) < 0)
        fail("glosses", error);
    show("imported from raw text", raw);

    char *toolbox = suz_toolbox(raw);
    printf("-- Toolbox\n%s\n", toolbox);
    suz_session *from_toolbox = suz_session_from_toolbox(language, toolbox, &error);
    if (!from_toolbox)
        fail("toolbox", error);
    show("read back from Toolbox", from_toolbox);

    suz_session_free(from_toolbox);
    suz_free(toolbox);
    suz_session_free(raw);
}
