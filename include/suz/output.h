/* SPDX-License-Identifier: GPL-3.0-or-later */
/**
 * @file suz/output.h
 * @brief Text forms of a session document.
 */
#ifndef SUZ_OUTPUT_H
#define SUZ_OUTPUT_H

#include "suz/core.h"

#ifdef __cplusplus
extern "C" {
#endif

/**
 * Interlinear text of the document: surface forms, morph forms and glosses
 * as three aligned lines per segment.
 * @param session Session.
 * @return Text, release with suz_free().
 */
char *suz_render(suz_session *session);

/**
 * Text snapshot of the document. The history is not included.
 * @param session Session.
 * @return Text, release with suz_free().
 */
char *suz_snapshot(suz_session *session);

/**
 * Journal text: the commands that produced the document, one per line, oldest
 * first. Undone commands are not included.
 * @param session Session.
 * @return Text, release with suz_free().
 */
char *suz_journal(suz_session *session);

/**
 * Toolbox text of the document: one record per segment with the \\ref, \\tx,
 * \\mb and \\ge fields. A backslash, hyphen or period in a form, a lexical
 * part or a tag is written after a backslash and whitespace as \\uHEX;, so the
 * text survives suz_session_from_toolbox(). An empty form or an empty gloss
 * does not.
 * @param session Session.
 * @return Text, release with suz_free().
 */
char *suz_toolbox(suz_session *session);

/**
 * CoNLL-U text of the document: one sentence per segment with the ID and FORM
 * columns filled and the others set to `_`. A token with several morphs is a
 * range line followed by a line per morph form. Segments without tokens are
 * skipped. Whitespace in a form is written as a no-break space and an empty
 * form as `_`. The text can be read with suz_session_from_conllu(), which
 * gives each word one unglossed morph, so glosses and the morph forms of
 * single-morph tokens are not kept.
 * @param session Session.
 * @return Text, release with suz_free().
 */
char *suz_conllu(suz_session *session);

/**
 * JSON text of the document on one line: an object with a `segments` list of
 * segments with `id` and `tokens`, tokens with `id`, `surface` and `morphs`,
 * and morphs with `id`, `form` and `gloss`. The gloss is `null` for a morph
 * without one, otherwise an object with `lexical` and `tags`. Identifiers are
 * numbers up to 2^64-1.
 * @param session Session.
 * @return Text, release with suz_free().
 */
char *suz_json(suz_session *session);

#ifdef __cplusplus
}
#endif

#endif
