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
 * Releases a language.
 * @param language Language to release.
 */
void suz_language_free(suz_language *language);

#ifdef __cplusplus
}
#endif

#endif
