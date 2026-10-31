/* SPDX-License-Identifier: GPL-3.0-or-later */
#include "suz-morph-editor.h"

typedef struct {
    SuzWindow *window;
    GtkWidget *popover;
    int segment;
    int token;
    int morph;
} Editor;

static char *join_gloss(const char *lexical, GStrv tags)
{
    GString *text = g_string_new(lexical ? lexical : "");
    for (int i = 0; tags && tags[i]; i++) {
        if (text->len > 0)
            g_string_append_c(text, '.');
        g_string_append(text, tags[i]);
    }
    return g_string_free(text, FALSE);
}

static char *current_gloss(suz_session *session, int s, int t, int m)
{
    if (suz_morph_glossed(session, s, t, m) != 1)
        return g_strdup("");

    char *lexical = suz_morph_lexical(session, s, t, m);
    int count = suz_morph_tag_count(session, s, t, m);
    GPtrArray *tags = g_ptr_array_new_with_free_func(g_free);
    for (int i = 0; i < count; i++) {
        char *tag = suz_morph_tag(session, s, t, m, i);
        g_ptr_array_add(tags, g_strdup(tag));
        suz_free(tag);
    }
    g_ptr_array_add(tags, NULL);
    char *text = join_gloss(lexical, (GStrv)tags->pdata);
    g_ptr_array_free(tags, TRUE);
    suz_free(lexical);
    return text;
}

typedef struct {
    SuzWindow *window;
    int segment;
    int token;
    int morph;
    gboolean form;
    char *text;
} Change;

static gboolean apply_change(gpointer data)
{
    Change *change = data;
    if (change->form)
        suz_window_set_morph_form(change->window, change->segment, change->token, change->morph, change->text);
    else
        suz_window_set_morph_gloss(change->window, change->segment, change->token, change->morph, change->text);
    g_object_unref(change->window);
    g_free(change->text);
    g_free(change);
    return G_SOURCE_REMOVE;
}

static void submit(Editor *editor, gboolean form, const char *text)
{
    Change *change = g_new0(Change, 1);
    change->window = g_object_ref(editor->window);
    change->segment = editor->segment;
    change->token = editor->token;
    change->morph = editor->morph;
    change->form = form;
    change->text = g_strdup(text);

    if (editor->popover)
        gtk_popover_popdown(GTK_POPOVER(editor->popover));
    g_idle_add(apply_change, change);
}

static void on_form_apply(AdwEntryRow *row, Editor *editor)
{
    submit(editor, TRUE, gtk_editable_get_text(GTK_EDITABLE(row)));
}

static void on_gloss_apply(AdwEntryRow *row, Editor *editor)
{
    submit(editor, FALSE, gtk_editable_get_text(GTK_EDITABLE(row)));
}

static void on_remove_gloss(GtkButton *button, Editor *editor)
{
    (void)button;
    submit(editor, FALSE, NULL);
}

static void on_candidate_activated(AdwActionRow *row, Editor *editor)
{
    submit(editor, FALSE, g_object_get_data(G_OBJECT(row), "gloss"));
}

static gboolean unparent_popover(gpointer data)
{
    GtkWidget *popover = GTK_WIDGET(data);
    if (gtk_widget_get_parent(popover))
        gtk_widget_unparent(popover);
    g_object_unref(popover);
    return G_SOURCE_REMOVE;
}

static void on_popover_closed(GtkPopover *popover, gpointer user_data)
{
    (void)user_data;
    g_idle_add(unparent_popover, g_object_ref(popover));
}

static void on_anchor_destroy(GtkWidget *anchor, gpointer user_data)
{
    GtkWidget *popover = GTK_WIDGET(user_data);
    if (gtk_widget_get_parent(popover) == anchor)
        gtk_widget_unparent(popover);
}

static GtkWidget *make_candidates(Editor *editor, suz_language *language, const char *form)
{
    int count = suz_language_candidate_count(language, form);
    if (count <= 0)
        return NULL;

    GtkWidget *list = gtk_list_box_new();
    gtk_list_box_set_selection_mode(GTK_LIST_BOX(list), GTK_SELECTION_NONE);
    gtk_widget_add_css_class(list, "boxed-list");

    for (int i = 0; i < count; i++) {
        char *lexical = suz_language_candidate_lexical(language, form, i);
        int tag_count = suz_language_candidate_tag_count(language, form, i);
        GPtrArray *tags = g_ptr_array_new_with_free_func(g_free);
        for (int t = 0; t < tag_count; t++) {
            char *tag = suz_language_candidate_tag(language, form, i, t);
            g_ptr_array_add(tags, g_strdup(tag));
            suz_free(tag);
        }
        g_ptr_array_add(tags, NULL);
        char *gloss = join_gloss(lexical, (GStrv)tags->pdata);
        g_ptr_array_free(tags, TRUE);
        suz_free(lexical);

        double weight = 0;
        suz_language_candidate_weight(language, form, i, &weight);
        g_autofree char *weight_text = g_strdup_printf("%g", weight);

        GtkWidget *row = adw_action_row_new();
        adw_preferences_row_set_use_markup(ADW_PREFERENCES_ROW(row), FALSE);
        adw_preferences_row_set_title(ADW_PREFERENCES_ROW(row), gloss);
        gtk_list_box_row_set_activatable(GTK_LIST_BOX_ROW(row), TRUE);

        GtkWidget *weight_label = gtk_label_new(weight_text);
        gtk_widget_add_css_class(weight_label, "dim-label");
        gtk_widget_add_css_class(weight_label, "numeric");
        adw_action_row_add_suffix(ADW_ACTION_ROW(row), weight_label);

        g_object_set_data_full(G_OBJECT(row), "gloss", gloss, g_free);
        g_signal_connect(row, "activated", G_CALLBACK(on_candidate_activated), editor);
        gtk_list_box_append(GTK_LIST_BOX(list), row);
    }
    return list;
}

void suz_morph_editor_popup(SuzWindow *window, GtkWidget *anchor, int segment, int token, int morph)
{
    suz_session *session = suz_window_get_session(window);
    suz_language *language = suz_window_get_language(window);
    if (!session)
        return;

    char *form = suz_morph_form(session, segment, token, morph);
    g_autofree char *gloss = current_gloss(session, segment, token, morph);
    gboolean glossed = suz_morph_glossed(session, segment, token, morph) == 1;

    Editor *editor = g_new0(Editor, 1);
    editor->window = window;
    editor->segment = segment;
    editor->token = token;
    editor->morph = morph;

    GtkWidget *box = gtk_box_new(GTK_ORIENTATION_VERTICAL, 12);
    gtk_widget_set_margin_top(box, 12);
    gtk_widget_set_margin_bottom(box, 12);
    gtk_widget_set_margin_start(box, 12);
    gtk_widget_set_margin_end(box, 12);
    gtk_widget_set_size_request(box, 340, -1);

    GtkWidget *fields = gtk_list_box_new();
    gtk_list_box_set_selection_mode(GTK_LIST_BOX(fields), GTK_SELECTION_NONE);
    gtk_widget_add_css_class(fields, "boxed-list");

    GtkWidget *form_row = adw_entry_row_new();
    adw_preferences_row_set_title(ADW_PREFERENCES_ROW(form_row), "Form");
    gtk_editable_set_text(GTK_EDITABLE(form_row), form);
    adw_entry_row_set_show_apply_button(ADW_ENTRY_ROW(form_row), TRUE);
    g_signal_connect(form_row, "apply", G_CALLBACK(on_form_apply), editor);
    gtk_list_box_append(GTK_LIST_BOX(fields), form_row);

    GtkWidget *gloss_row = adw_entry_row_new();
    adw_preferences_row_set_title(ADW_PREFERENCES_ROW(gloss_row), "Gloss");
    gtk_editable_set_text(GTK_EDITABLE(gloss_row), gloss);
    adw_entry_row_set_show_apply_button(ADW_ENTRY_ROW(gloss_row), TRUE);
    g_signal_connect(gloss_row, "apply", G_CALLBACK(on_gloss_apply), editor);
    gtk_list_box_append(GTK_LIST_BOX(fields), gloss_row);
    gtk_box_append(GTK_BOX(box), fields);

    GtkWidget *hint = gtk_label_new("Separate the parts of a gloss with periods, for example book.PL.");
    gtk_label_set_wrap(GTK_LABEL(hint), TRUE);
    gtk_label_set_xalign(GTK_LABEL(hint), 0.0f);
    gtk_widget_add_css_class(hint, "caption");
    gtk_widget_add_css_class(hint, "dim-label");
    gtk_box_append(GTK_BOX(box), hint);

    if (language) {
        GtkWidget *candidates = make_candidates(editor, language, form);
        if (candidates) {
            GtkWidget *heading = gtk_label_new("Suggestions");
            gtk_label_set_xalign(GTK_LABEL(heading), 0.0f);
            gtk_widget_add_css_class(heading, "heading");
            gtk_box_append(GTK_BOX(box), heading);
            gtk_box_append(GTK_BOX(box), candidates);
        }
    }

    if (glossed) {
        GtkWidget *remove = gtk_button_new_with_label("Remove Gloss");
        gtk_widget_add_css_class(remove, "destructive-action");
        gtk_widget_set_halign(remove, GTK_ALIGN_START);
        g_signal_connect(remove, "clicked", G_CALLBACK(on_remove_gloss), editor);
        gtk_box_append(GTK_BOX(box), remove);
    }

    GtkWidget *popover = gtk_popover_new();
    editor->popover = popover;
    g_object_set_data_full(G_OBJECT(popover), "editor", editor, g_free);
    g_object_add_weak_pointer(G_OBJECT(popover), (gpointer *)&editor->popover);
    gtk_popover_set_child(GTK_POPOVER(popover), box);
    gtk_widget_set_parent(popover, anchor);
    g_signal_connect(popover, "closed", G_CALLBACK(on_popover_closed), NULL);
    g_signal_connect_object(anchor, "destroy", G_CALLBACK(on_anchor_destroy), popover, 0);
    gtk_popover_popup(GTK_POPOVER(popover));
    gtk_widget_grab_focus(gloss_row);

    suz_free(form);
}
