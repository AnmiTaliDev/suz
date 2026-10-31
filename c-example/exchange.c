/* SPDX-License-Identifier: GPL-3.0-or-later */
#include <stdio.h>

#include "example.h"

void demo_exchange(suz_language *language)
{
    char *error = NULL;

    suz_session *raw = suz_session_from_text_with_language(language, "Мин китапларны укыдым.\nМин укыдым\n", &error);
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

    char *leipzig = suz_render(raw);
    suz_session *from_leipzig = suz_session_from_leipzig(language, leipzig, &error);
    if (!from_leipzig)
        fail("leipzig", error);
    show("read back from interlinear text", from_leipzig);

    suz_session *from_conllu = suz_session_from_conllu(
        "# text = Мин укыдым\n"
        "1\tМин\tмин\tPRON\t_\t_\t2\tnsubj\t_\t_\n"
        "2\tукыдым\tукы\tVERB\t_\t_\t0\troot\t_\t_\n",
        &error);
    if (!from_conllu)
        fail("conllu", error);
    show("imported from CoNLL-U", from_conllu);

    char *conllu = suz_conllu(raw);
    printf("-- CoNLL-U\n%s", conllu);
    suz_free(conllu);

    char *json = suz_json(raw);
    printf("-- JSON\n%s\n", json);
    suz_free(json);

    suz_session_free(from_conllu);
    suz_session_free(from_leipzig);
    suz_free(leipzig);
    suz_session_free(from_toolbox);
    suz_free(toolbox);
    suz_session_free(raw);
}
