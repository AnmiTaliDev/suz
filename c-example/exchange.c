/* SPDX-License-Identifier: GPL-3.0-or-later */
#include <inttypes.h>
#include <stdio.h>

#include "example.h"

static void print_ud(suz_session *session)
{
    printf("-- UD annotation\n");
    int segments = suz_segment_count(session);
    for (int s = 0; s < segments; s++) {
        int tokens = suz_token_count(session, s);
        for (int t = 0; t < tokens; t++) {
            int morphs = suz_morph_count(session, s, t);
            for (int m = 0; m < morphs; m++) {
                if (suz_morph_has_ud(session, s, t, m) != 1)
                    continue;
                char *form = suz_morph_form(session, s, t, m);
                char *upos = suz_morph_ud_text(session, s, t, m, SUZ_UD_UPOS);
                char *deprel = suz_morph_ud_text(session, s, t, m, SUZ_UD_DEPREL);
                uint64_t head = 0;
                int kind = suz_morph_ud_head(session, s, t, m, &head);
                printf("  %s %s %s", form, upos, deprel);
                if (kind == 1)
                    printf(" root");
                else if (kind == 2)
                    printf(" head %" PRIu64, head);
                printf("\n");
                suz_free(form);
                suz_free(upos);
                suz_free(deprel);
            }
        }
    }
}

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
    print_ud(from_conllu);

    uint64_t verb;
    suz_morph_id(from_conllu, 0, 1, 0, &verb);
    if (suz_set_ud(from_conllu, 0, 0, 0, "мин", "PRON", "", "Case=Nom", 2, verb, "nsubj", "", "SpaceAfter=No", &error) < 0)
        fail("set ud", error);
    char *annotated = suz_conllu(from_conllu);
    printf("-- CoNLL-U after suz_set_ud\n%s", annotated);
    suz_free(annotated);

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
