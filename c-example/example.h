/* SPDX-License-Identifier: GPL-3.0-or-later */
#ifndef EXAMPLE_H
#define EXAMPLE_H

#include "suz.h"

void fail(const char *what, char *error);
void show(const char *title, suz_session *session);
suz_session *session_with_word(const char *word);

void demo_editing(suz_language *language);
void demo_learning(suz_language *language);
void demo_exchange(suz_language *language);

#endif
