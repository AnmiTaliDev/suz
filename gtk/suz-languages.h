/* SPDX-License-Identifier: GPL-3.0-or-later */
#pragma once

#include <glib.h>

#include "suz.h"

typedef struct {
    char *name;
    char *path;
} SuzLanguageInfo;

void suz_language_info_free(SuzLanguageInfo *info);
GPtrArray *suz_languages_discover(void);
suz_language *suz_languages_load(const SuzLanguageInfo *info, GError **error);
