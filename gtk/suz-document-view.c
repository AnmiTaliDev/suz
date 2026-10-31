/* SPDX-License-Identifier: GPL-3.0-or-later */
#include "suz-document-view.h"

#include <glib/gi18n.h>

struct _SuzDocumentView {
    AdwBin parent_instance;

    GtkScrolledWindow *scrolled;
    GtkBox *segments;
};

G_DEFINE_FINAL_TYPE(SuzDocumentView, suz_document_view, ADW_TYPE_BIN)

enum { SIGNAL_MORPH_ACTIVATED, N_SIGNALS };
static guint signals[N_SIGNALS];

typedef struct {
    SuzDocumentView *view;
    int segment;
    int token;
    int morph;
} MorphPosition;

static void free_position(gpointer data, GClosure *closure)
{
    (void)closure;
    g_free(data);
}

static void on_morph_clicked(GtkButton *button, MorphPosition *position)
{
    g_signal_emit(position->view, signals[SIGNAL_MORPH_ACTIVATED], 0,
                  position->segment, position->token, position->morph, button);
}

static char *gloss_markup(suz_session *session, int s, int t, int m)
{
    char *lexical = suz_morph_lexical(session, s, t, m);
    int tags = suz_morph_tag_count(session, s, t, m);
    GString *markup = g_string_new(NULL);

    if (lexical && *lexical) {
        g_autofree char *escaped = g_markup_escape_text(lexical, -1);
        g_string_append(markup, escaped);
    }
    for (int i = 0; i < tags; i++) {
        char *tag = suz_morph_tag(session, s, t, m, i);
        g_autofree char *escaped = g_markup_escape_text(tag, -1);
        if (markup->len > 0)
            g_string_append_c(markup, '.');
        g_string_append_printf(markup, "<span font_features=\"c2sc, smcp\" size=\"90%%\" letter_spacing=\"400\">%s</span>", escaped);
        suz_free(tag);
    }
    suz_free(lexical);
    return g_string_free(markup, FALSE);
}

static GtkWidget *make_label(const char *text, const char *css_class)
{
    GtkWidget *label = gtk_label_new(text);
    gtk_label_set_xalign(GTK_LABEL(label), 0.5f);
    if (css_class)
        gtk_widget_add_css_class(label, css_class);
    return label;
}

static GtkWidget *make_morph(SuzDocumentView *self, suz_session *session, int s, int t, int m)
{
    char *form = suz_morph_form(session, s, t, m);
    gboolean glossed = suz_morph_glossed(session, s, t, m) == 1;

    GtkWidget *form_label = make_label(form, "suz-form");
    GtkWidget *gloss_label;
    if (glossed) {
        g_autofree char *markup = gloss_markup(session, s, t, m);
        gloss_label = make_label(NULL, "suz-gloss");
        gtk_label_set_markup(GTK_LABEL(gloss_label), markup);
    } else {
        gloss_label = make_label("?", "suz-gloss");
        gtk_widget_add_css_class(gloss_label, "dim-label");
    }

    GtkWidget *column = gtk_box_new(GTK_ORIENTATION_VERTICAL, 2);
    gtk_box_append(GTK_BOX(column), form_label);
    gtk_box_append(GTK_BOX(column), gloss_label);

    GtkWidget *button = gtk_button_new();
    gtk_button_set_child(GTK_BUTTON(button), column);
    gtk_widget_add_css_class(button, "flat");
    gtk_widget_add_css_class(button, "suz-morph");
    gtk_widget_set_valign(button, GTK_ALIGN_START);

    g_autofree char *description = g_strdup_printf("%s, %s", form, glossed ? gtk_label_get_text(GTK_LABEL(gloss_label)) : _("no gloss"));
    gtk_accessible_update_property(GTK_ACCESSIBLE(button), GTK_ACCESSIBLE_PROPERTY_LABEL, description, -1);

    MorphPosition *position = g_new0(MorphPosition, 1);
    position->view = self;
    position->segment = s;
    position->token = t;
    position->morph = m;
    g_signal_connect_data(button, "clicked", G_CALLBACK(on_morph_clicked), position, free_position, 0);

    suz_free(form);
    return button;
}

static GtkWidget *make_hyphen(void)
{
    GtkWidget *column = gtk_box_new(GTK_ORIENTATION_VERTICAL, 2);
    gtk_box_append(GTK_BOX(column), make_label("-", "suz-form"));
    gtk_box_append(GTK_BOX(column), make_label("-", "suz-gloss"));
    gtk_widget_add_css_class(column, "suz-hyphen");
    gtk_widget_set_valign(column, GTK_ALIGN_START);
    return column;
}

static GtkWidget *make_token(SuzDocumentView *self, suz_session *session, int s, int t)
{
    char *surface = suz_token_surface(session, s, t);
    GtkWidget *surface_label = make_label(surface, "suz-surface");
    gtk_widget_set_margin_bottom(surface_label, 2);
    suz_free(surface);

    GtkWidget *morphs = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
    int count = suz_morph_count(session, s, t);
    for (int m = 0; m < count; m++) {
        if (m > 0)
            gtk_box_append(GTK_BOX(morphs), make_hyphen());
        gtk_box_append(GTK_BOX(morphs), make_morph(self, session, s, t, m));
    }

    GtkWidget *token = gtk_box_new(GTK_ORIENTATION_VERTICAL, 2);
    gtk_box_append(GTK_BOX(token), surface_label);
    gtk_box_append(GTK_BOX(token), morphs);
    gtk_widget_set_halign(morphs, GTK_ALIGN_CENTER);
    return token;
}

static GtkWidget *make_segment(SuzDocumentView *self, suz_session *session, int s)
{
    GtkWidget *wrap = adw_wrap_box_new();
    adw_wrap_box_set_child_spacing(ADW_WRAP_BOX(wrap), 18);
    adw_wrap_box_set_line_spacing(ADW_WRAP_BOX(wrap), 14);
    gtk_widget_set_hexpand(wrap, TRUE);

    int tokens = suz_token_count(session, s);
    for (int t = 0; t < tokens; t++)
        adw_wrap_box_append(ADW_WRAP_BOX(wrap), make_token(self, session, s, t));

    g_autofree char *number = g_strdup_printf("%d", s + 1);
    GtkWidget *number_label = gtk_label_new(number);
    gtk_widget_add_css_class(number_label, "suz-number");
    gtk_widget_add_css_class(number_label, "dim-label");
    gtk_widget_set_valign(number_label, GTK_ALIGN_START);
    gtk_label_set_xalign(GTK_LABEL(number_label), 1.0f);

    GtkWidget *card = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 16);
    gtk_widget_add_css_class(card, "card");
    gtk_widget_add_css_class(card, "suz-segment");
    gtk_box_append(GTK_BOX(card), number_label);
    gtk_box_append(GTK_BOX(card), wrap);
    return card;
}

static gboolean restore_scroll(gpointer data)
{
    gpointer *state = data;
    gtk_adjustment_set_value(GTK_ADJUSTMENT(state[0]), *(double *)state[1]);
    g_object_unref(state[0]);
    g_free(state[1]);
    g_free(state);
    return G_SOURCE_REMOVE;
}

void suz_document_view_set_session(SuzDocumentView *self, suz_session *session)
{
    g_return_if_fail(SUZ_IS_DOCUMENT_VIEW(self));

    GtkAdjustment *adjustment = gtk_scrolled_window_get_vadjustment(self->scrolled);
    double value = gtk_adjustment_get_value(adjustment);

    GtkWidget *child;
    while ((child = gtk_widget_get_first_child(GTK_WIDGET(self->segments))))
        gtk_box_remove(self->segments, child);

    if (session) {
        int segments = suz_segment_count(session);
        for (int s = 0; s < segments; s++)
            gtk_box_append(self->segments, make_segment(self, session, s));
    }

    gpointer *state = g_new0(gpointer, 2);
    state[0] = g_object_ref(adjustment);
    state[1] = g_memdup2(&value, sizeof value);
    g_idle_add(restore_scroll, state);
}

static void suz_document_view_class_init(SuzDocumentViewClass *klass)
{
    signals[SIGNAL_MORPH_ACTIVATED] = g_signal_new("morph-activated", G_TYPE_FROM_CLASS(klass), G_SIGNAL_RUN_LAST,
                                                   0, NULL, NULL, NULL, G_TYPE_NONE, 4,
                                                   G_TYPE_INT, G_TYPE_INT, G_TYPE_INT, GTK_TYPE_WIDGET);
}

static void suz_document_view_init(SuzDocumentView *self)
{
    self->segments = GTK_BOX(gtk_box_new(GTK_ORIENTATION_VERTICAL, 12));
    gtk_widget_set_margin_top(GTK_WIDGET(self->segments), 18);
    gtk_widget_set_margin_bottom(GTK_WIDGET(self->segments), 24);
    gtk_widget_set_margin_start(GTK_WIDGET(self->segments), 18);
    gtk_widget_set_margin_end(GTK_WIDGET(self->segments), 18);

    GtkWidget *clamp = adw_clamp_new();
    adw_clamp_set_maximum_size(ADW_CLAMP(clamp), 960);
    adw_clamp_set_tightening_threshold(ADW_CLAMP(clamp), 720);
    adw_clamp_set_child(ADW_CLAMP(clamp), GTK_WIDGET(self->segments));

    self->scrolled = GTK_SCROLLED_WINDOW(gtk_scrolled_window_new());
    gtk_scrolled_window_set_policy(self->scrolled, GTK_POLICY_NEVER, GTK_POLICY_AUTOMATIC);
    gtk_scrolled_window_set_child(self->scrolled, clamp);

    adw_bin_set_child(ADW_BIN(self), GTK_WIDGET(self->scrolled));
}

SuzDocumentView *suz_document_view_new(void)
{
    return g_object_new(SUZ_TYPE_DOCUMENT_VIEW, NULL);
}
