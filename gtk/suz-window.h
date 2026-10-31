/* SPDX-License-Identifier: GPL-3.0-or-later */
#pragma once

#include <adwaita.h>

#include "suz.h"

G_BEGIN_DECLS

#define SUZ_TYPE_WINDOW (suz_window_get_type())
G_DECLARE_FINAL_TYPE(SuzWindow, suz_window, SUZ, WINDOW, AdwApplicationWindow)

SuzWindow *suz_window_new(AdwApplication *application);
void suz_window_open_file(SuzWindow *self, GFile *file);

suz_session *suz_window_get_session(SuzWindow *self);
suz_language *suz_window_get_language(SuzWindow *self);

gboolean suz_window_set_morph_form(SuzWindow *self, int segment, int token, int morph, const char *form);
gboolean suz_window_set_morph_gloss(SuzWindow *self, int segment, int token, int morph, const char *gloss);

G_END_DECLS
