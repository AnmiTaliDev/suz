/* SPDX-License-Identifier: GPL-3.0-or-later */
/**
 * @file suz.h
 * @brief C interface of the Suz interlinear glossing engine.
 *
 * Strings passed in are NUL-terminated UTF-8. Strings returned and error
 * messages are allocated with malloc and must be released with suz_free().
 * Fallible functions return 0 on success and -1 on failure, constructors
 * return NULL on failure. Each of them takes an optional `char **error`
 * argument: pass NULL to ignore the message, otherwise a message is stored
 * there on failure and the pointer is left untouched on success.
 *
 * The library keeps no global error state. It is not thread-safe: calls must
 * come from one thread or be serialized by the caller.
 */
#ifndef SUZ_H
#define SUZ_H

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/** Document together with its undo and redo history. */
typedef struct suz_session suz_session;

/** Weight tables of one language. */
typedef struct suz_language suz_language;

/**
 * Starts the Haskell runtime. Call once, before any other function.
 * @return 0 on success.
 */
int suz_init(void);

/**
 * Stops the Haskell runtime. Free all sessions and languages first. The
 * runtime cannot be started again afterwards.
 */
void suz_exit(void);

/**
 * Releases a string returned by this library.
 * @param p String to release, may be NULL.
 */
void suz_free(void *p);

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
 * Applies one editing command like suz_perform() and, when it succeeds,
 * updates the weights of the language from the glosses it set, replaced or
 * cleared. Use it for edits made by a person. Suggestions applied with
 * suz_suggest_glosses() do not change the language. Undoing the command does
 * not revert the language.
 * @param session Session.
 * @param language Language to update.
 * @param command One command in the line format of suz_journal().
 * @param error Receives the message on failure, may be NULL.
 * @return 0 on success, -1 on failure. Session and language are unchanged on
 *         failure.
 */
int suz_perform_learning(suz_session *session, suz_language *language, const char *command, char **error);

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
 * \\mb and \\ge fields. The text is not escaped, so surface forms with
 * whitespace and morph forms with hyphens do not survive a round trip through
 * suz_session_from_toolbox().
 * @param session Session.
 * @return Text, release with suz_free().
 */
char *suz_toolbox(suz_session *session);

/**
 * Reads a language from its text form.
 * @param text Text form.
 * @param error Receives the message on failure, may be NULL.
 * @return New language, release with suz_language_free(), or NULL.
 */
suz_language *suz_language_from_text(const char *text, char **error);

/**
 * Reads a language from its binary form.
 * @param data Binary form.
 * @param size Size of data in bytes.
 * @param error Receives the message on failure, may be NULL.
 * @return New language, release with suz_language_free(), or NULL.
 */
suz_language *suz_language_from_binary(const uint8_t *data, size_t size, char **error);

/**
 * Text form of a language, including the weights changed by
 * suz_perform_learning().
 * @param language Language.
 * @return Text, release with suz_free().
 */
char *suz_language_to_text(const suz_language *language);

/**
 * Binary form of a language, including the weights changed by
 * suz_perform_learning().
 * @param language Language.
 * @param size Receives the size of the result in bytes.
 * @return Bytes, release with suz_free().
 */
uint8_t *suz_language_to_binary(const suz_language *language, size_t *size);

/**
 * Releases a language.
 * @param language Language to release.
 */
void suz_language_free(suz_language *language);

/**
 * Splits unglossed single-morph tokens into the morphs known to the language
 * and applies the result as one command, so one suz_undo() reverts it.
 * @param session Session.
 * @param language Language.
 * @param error Receives the message on failure, may be NULL.
 * @return Number of editing commands applied (0 when there was nothing to
 *         suggest), or -1 on failure.
 */
int suz_suggest_segmentation(suz_session *session, const suz_language *language, char **error);

/**
 * Glosses unglossed morphs with the heaviest candidate of the language and
 * applies the result as one command, so one suz_undo() reverts it.
 * @param session Session.
 * @param language Language.
 * @param error Receives the message on failure, may be NULL.
 * @return Number of glosses set (0 when there was nothing to suggest), or -1
 *         on failure.
 */
int suz_suggest_glosses(suz_session *session, const suz_language *language, char **error);

#ifdef __cplusplus
}
#endif

#endif
