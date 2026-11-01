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
 * Interlinear text like suz_render(), with the lines broken so that none is
 * longer than the given number of terminal columns, except where a single word
 * is wider. The groups of three lines are separated by a blank line. Width
 * counts terminal columns: combining marks and the zero-width non-joiner take
 * none and East Asian wide characters take two. The text is for reading, it is
 * not suited for suz_session_from_leipzig() because a wrapped segment becomes
 * several.
 * @param session Session.
 * @param width Number of columns.
 * @return Text, release with suz_free().
 */
char *suz_render_wrapped(suz_session *session, int width);

/**
 * Text snapshot of the document. The history is not included. The first line,
 * `suz-snapshot` and a tab and the format version, names the version of the
 * format, which is 1.
 * @param session Session.
 * @return Text, release with suz_free().
 */
char *suz_snapshot(suz_session *session);

/**
 * Journal text: a first line, `suz-journal` and a tab and the format version,
 * which is 1, then the commands that produced the document, one per line,
 * oldest first. Undone commands are not included.
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
 * columns filled and the UD annotation of the morphs in the others, `_` where
 * there is none. A token with several morphs is a range line followed by a
 * line per morph form. The gloss of a morph is a `Gloss=` item at the end of
 * its MISC column. Segments without tokens are skipped. Whitespace in a form
 * is written as a no-break space and an empty form as `_`. The text can be
 * read with suz_session_from_conllu_with_language(), which restores the
 * glosses. The morph forms of single-morph tokens are not kept.
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

/**
 * LaTeX source of the document for the gb4e package: one exe environment with
 * a three-line \glll example for each segment that has tokens. Tags are set in
 * lowercase small capitals, special characters are escaped. Empty text when
 * no segment has tokens.
 * @param session Session.
 * @return Text, release with suz_free().
 */
char *suz_latex(suz_session *session);

/**
 * A complete HTML page of the document with its style sheet inside. Each
 * segment that has tokens is a list item, each token a word with its surface
 * form, its morph forms and its glosses, tags are set in small capitals. A
 * segment takes the direction of its first strong character.
 * @param session Session.
 * @return Text, release with suz_free().
 */
char *suz_html(suz_session *session);

/**
 * An ELAN annotation file (EAF 3.0). Each segment that has tokens is an
 * annotation of the `segment` tier with a made-up time of one second. The
 * `word`, `morph` and `gloss` tiers hold the surface forms, the morph forms
 * and the glosses below it, a gloss is its lexical part and its tags joined by
 * periods, and a morph without a gloss has none. The text is not read back.
 * @param session Session.
 * @return Text, release with suz_free().
 */
char *suz_elan(suz_session *session);

/**
 * A FLExText file (`.flextext`) for the FieldWorks import of interlinear
 * texts. The document is one text with one paragraph, each segment that has
 * tokens is a phrase, each token a word with its morphs. A gloss is its lexical
 * part and its tags joined by periods, a morph without a gloss has none, and a
 * token of only punctuation or symbols is a punctuation word. The text is not
 * read back.
 * @param session Session.
 * @param vernacular Tag of the writing system of the text, such as `kk`.
 * @param analysis Tag of the writing system of the glosses, such as `en`.
 * @return Text, release with suz_free(), or NULL if an argument is not valid
 *         UTF-8.
 */
char *suz_flex(suz_session *session, const char *vernacular, const char *analysis);

/**
 * Problems with the UD heads of the document, one per line with tab-separated
 * fields: `dangling`, the morph and a head that does not exist, `outside`,
 * the morph and a head in another segment, or `cycle` and a morph on a cycle
 * of heads. The text is empty if there are none. Commands do not create such
 * heads, they come from a snapshot or CoNLL-U text that was read.
 * @param session Session.
 * @return Text, release with suz_free().
 */
char *suz_validate_ud(suz_session *session);

#ifdef __cplusplus
}
#endif

#endif
