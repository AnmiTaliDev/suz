/* SPDX-License-Identifier: GPL-3.0-or-later */
/**
 * @file suz/learning.h
 * @brief Suggestions and learning from edits.
 */
#ifndef SUZ_LEARNING_H
#define SUZ_LEARNING_H

#include "suz/core.h"

#ifdef __cplusplus
extern "C" {
#endif

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
