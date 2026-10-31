/* SPDX-License-Identifier: GPL-3.0-or-later */
/**
 * @file suz/language.h
 * @brief Reading and writing languages.
 */
#ifndef SUZ_LANGUAGE_H
#define SUZ_LANGUAGE_H

#include "suz/core.h"

#ifdef __cplusplus
extern "C" {
#endif

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
 * @param language Language.
 * @return Name of the language, release with suz_free().
 */
char *suz_language_name(const suz_language *language);

/**
 * @param language Language.
 * @return Number of grammatical tags the language declares.
 */
int suz_language_tag_count(const suz_language *language);

/**
 * @param language Language.
 * @param index Tag index, tags are in ascending order.
 * @return Grammatical tag, release with suz_free(), or NULL if the index is
 * out of range.
 */
char *suz_language_tag(const suz_language *language, int index);

/**
 * Candidate glosses of a morph form, from the heaviest to the lightest.
 * @param language Language.
 * @param form Morph form.
 * @return Number of candidates, 0 if the form is unknown, -1 if the form is
 * not valid UTF-8.
 */
int suz_language_candidate_count(const suz_language *language, const char *form);

/**
 * @param language Language.
 * @param form Morph form.
 * @param index Candidate index.
 * @param weight Receives the weight of the candidate.
 * @return 0 on success, -1 if the candidate does not exist.
 */
int suz_language_candidate_weight(const suz_language *language, const char *form, int index, double *weight);

/**
 * @param language Language.
 * @param form Morph form.
 * @param index Candidate index.
 * @return Lexical part of the candidate gloss, empty for a purely
 * grammatical gloss, release with suz_free(), or NULL if the candidate does
 * not exist.
 */
char *suz_language_candidate_lexical(const suz_language *language, const char *form, int index);

/**
 * @param language Language.
 * @param form Morph form.
 * @param index Candidate index.
 * @return Number of grammatical tags of the candidate gloss, -1 if the
 * candidate does not exist.
 */
int suz_language_candidate_tag_count(const suz_language *language, const char *form, int index);

/**
 * @param language Language.
 * @param form Morph form.
 * @param index Candidate index.
 * @param tag Tag index.
 * @return Grammatical tag of the candidate gloss, release with suz_free(), or
 * NULL if the candidate or the tag does not exist.
 */
char *suz_language_candidate_tag(const suz_language *language, const char *form, int index, int tag);

/**
 * Releases a language.
 * @param language Language to release.
 */
void suz_language_free(suz_language *language);

#ifdef __cplusplus
}
#endif

#endif
