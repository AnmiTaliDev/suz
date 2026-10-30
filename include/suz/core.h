/* SPDX-License-Identifier: GPL-3.0-or-later */
/**
 * @file suz/core.h
 * @brief Runtime control, shared types and string release.
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
#ifndef SUZ_CORE_H
#define SUZ_CORE_H

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

#ifdef __cplusplus
}
#endif

#endif
