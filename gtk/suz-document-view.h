/* SPDX-License-Identifier: GPL-3.0-or-later */
#pragma once

#include <adwaita.h>

#include "suz.h"

G_BEGIN_DECLS

#define SUZ_TYPE_DOCUMENT_VIEW (suz_document_view_get_type())
G_DECLARE_FINAL_TYPE(SuzDocumentView, suz_document_view, SUZ, DOCUMENT_VIEW, AdwBin)

SuzDocumentView *suz_document_view_new(void);
void suz_document_view_set_session(SuzDocumentView *self, suz_session *session);

G_END_DECLS
