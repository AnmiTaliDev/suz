/* SPDX-License-Identifier: GPL-3.0-or-later */
/**
 * @file suz/session.h
 * @brief Creating, editing and undoing sessions.
 */
#ifndef SUZ_SESSION_H
#define SUZ_SESSION_H

#include "suz/core.h"

#ifdef __cplusplus
extern "C" {
#endif

/**
 * Creates a session over an empty document.
 * @return New session, release with suz_session_free().
 */
suz_session *suz_session_new(void);

/**
 * Creates a session from the text produced by suz_snapshot(). The history of
 * the new session is empty.
 * @param text Snapshot text.
 * @param error Receives the message on failure, may be NULL.
 * @return New session or NULL.
 */
suz_session *suz_session_from_snapshot(const char *text, char **error);

/**
 * Creates a session by applying the commands of a text produced by
 * suz_journal() to an empty document.
 * @param text Journal text.
 * @param error Receives the message on failure, may be NULL.
 * @return New session or NULL.
 */
suz_session *suz_session_from_journal(const char *text, char **error);

/**
 * Creates a session from raw text. Every non-empty line becomes a segment,
 * every whitespace-separated word becomes a token with one unglossed morph
 * whose form is the word. Punctuation stays attached to its word. The history
 * of the new session is empty.
 * @param text Raw text.
 * @param error Receives the message on failure, may be NULL.
 * @return New session or NULL.
 */
suz_session *suz_session_from_text(const char *text, char **error);

/**
 * Creates a session from Toolbox text as written by suz_toolbox(). Records
 * without a \\tx line are skipped. A gloss is split at periods: the first part
 * is a tag when the language lists it and the lexical part otherwise. The
 * history of the new session is empty and the language is not changed.
 * @param language Language that supplies the tag list.
 * @param text Toolbox text.
 * @param error Receives the message on failure, may be NULL.
 * @return New session or NULL.
 */
suz_session *suz_session_from_toolbox(const suz_language *language, const char *text, char **error);

/**
 * Creates a session from interlinear text as written by suz_render() and by
 * the Leipzig Glossing Rules: blocks separated by blank lines, each with a
 * line of words, a line of morph forms and a line of glosses. A fourth line
 * is ignored. Morphs are separated by hyphens, `_` is a word without morphs
 * and `?` is an unglossed morph. A gloss is split at periods: the first part
 * is a tag when the language lists it and the lexical part otherwise. The
 * history of the new session is empty and the language is not changed.
 * @param language Language that supplies the tag list.
 * @param text Interlinear text.
 * @param error Receives the message on failure, may be NULL.
 * @return New session or NULL.
 */
suz_session *suz_session_from_leipzig(const suz_language *language, const char *text, char **error);

/**
 * Creates a session from CoNLL-U text. Every sentence becomes a segment and
 * every word a token with one unglossed morph. A multiword token range
 * becomes one token with a morph per covered word. Only the ID and FORM
 * columns are read. The history of the new session is empty.
 * @param text CoNLL-U text.
 * @param error Receives the message on failure, may be NULL.
 * @return New session or NULL.
 */
suz_session *suz_session_from_conllu(const char *text, char **error);

/**
 * Releases a session.
 * @param session Session to release.
 */
void suz_session_free(suz_session *session);

/**
 * Smallest identifier never used in the session document. Segments, tokens
 * and morphs share one numbering. Nodes inserted by a command carry their own
 * identifiers, which must be unused.
 * @param session Session.
 * @return Next free identifier.
 */
uint64_t suz_next_id(suz_session *session);

/**
 * Applies one editing command and records it in the journal.
 * @param session Session.
 * @param command One command in the line format of suz_journal(), without
 *                the trailing newline.
 * @param error Receives the message on failure, may be NULL.
 * @return 0 on success, -1 on failure. The session is unchanged on failure.
 */
int suz_perform(suz_session *session, const char *command, char **error);

/**
 * Reverts the most recent recorded command.
 * @param session Session.
 * @param error Receives the message on failure, may be NULL.
 * @return 0 on success, -1 when there is nothing to undo or on failure.
 */
int suz_undo(suz_session *session, char **error);

/**
 * Reapplies the most recently undone command.
 * @param session Session.
 * @param error Receives the message on failure, may be NULL.
 * @return 0 on success, -1 when there is nothing to redo or on failure.
 */
int suz_redo(suz_session *session, char **error);

#ifdef __cplusplus
}
#endif

#endif
