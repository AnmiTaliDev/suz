/* SPDX-License-Identifier: GPL-3.0-or-later */
#include "suz-window.h"

#include <errno.h>
#include <glib/gstdio.h>
#include <inttypes.h>
#include <string.h>

#include "suz-document-view.h"
#include "suz-languages.h"
#include "suz-morph-editor.h"

struct _SuzWindow {
    AdwApplicationWindow parent_instance;

    AdwToastOverlay *toasts;
    GtkStack *stack;
    SuzDocumentView *view;
    AdwWindowTitle *title;
    GMenu *language_menu;

    GPtrArray *languages;
    suz_language *language;
    char *language_name;
    char *language_file;

    suz_session *session;
    GFile *file;
    char *document_name;
    gboolean dirty;
    GArray *undo_groups;
    GArray *redo_groups;
    gboolean close_after_save;
    gboolean close_confirmed;
};

G_DEFINE_FINAL_TYPE(SuzWindow, suz_window, ADW_TYPE_APPLICATION_WINDOW)

static const char *const export_formats[][3] = {
    {"interlinear", "Interlinear Text…", "txt"},
    {"toolbox", "Toolbox…", "txt"},
    {"conllu", "CoNLL-U…", "conllu"},
    {"json", "JSON…", "json"},
    {"latex", "LaTeX (gb4e)…", "tex"},
    {"html", "HTML…", "html"},
};

static void show_toast(SuzWindow *self, const char *message)
{
    AdwToast *toast = adw_toast_new(message);
    adw_toast_set_use_markup(toast, FALSE);
    adw_toast_overlay_add_toast(self->toasts, toast);
}

static void show_error(SuzWindow *self, const char *what, const char *detail)
{
    g_autofree char *message = g_strdup_printf("%s: %s", what, detail ? detail : "unknown error");
    show_toast(self, message);
}

static void set_action_enabled(SuzWindow *self, const char *name, gboolean enabled)
{
    GAction *action = g_action_map_lookup_action(G_ACTION_MAP(self), name);
    if (action)
        g_simple_action_set_enabled(G_SIMPLE_ACTION(action), enabled);
}

static char *user_language_path(SuzWindow *self)
{
    if (!self->language_file)
        return NULL;
    return g_build_filename(g_get_user_data_dir(), "suz", "languages", self->language_file, NULL);
}

static gboolean has_learned_weights(SuzWindow *self)
{
    g_autofree char *path = user_language_path(self);
    return path && g_file_test(path, G_FILE_TEST_IS_REGULAR);
}

static void update_ui(SuzWindow *self)
{
    gboolean has_document = self->session != NULL;

    set_action_enabled(self, "undo", self->undo_groups->len > 0);
    set_action_enabled(self, "redo", self->redo_groups->len > 0);
    set_action_enabled(self, "save", has_document);
    set_action_enabled(self, "save-as", has_document);
    set_action_enabled(self, "export", has_document);
    set_action_enabled(self, "annotate", has_document && self->language);
    set_action_enabled(self, "reset-language", has_learned_weights(self));

    gtk_stack_set_visible_child_name(self->stack, has_document ? "document" : "empty");

    g_autofree char *title = NULL;
    if (has_document)
        title = g_strdup_printf("%s%s", self->dirty ? "• " : "", self->document_name ? self->document_name : "Untitled");
    adw_window_title_set_title(self->title, title ? title : "Suz");
    adw_window_title_set_subtitle(self->title, self->language_name ? self->language_name : "");
}

static void refresh_view(SuzWindow *self)
{
    suz_document_view_set_session(self->view, self->session);
    update_ui(self);
}

static void clear_groups(GArray *groups)
{
    g_array_set_size(groups, 0);
}

static void push_group(GArray *groups, guint steps)
{
    g_array_append_val(groups, steps);
}

static void replace_session(SuzWindow *self, suz_session *session, GFile *file, const char *name, gboolean dirty)
{
    if (self->session)
        suz_session_free(self->session);
    self->session = session;

    g_clear_object(&self->file);
    if (file)
        self->file = g_object_ref(file);
    g_free(self->document_name);
    self->document_name = g_strdup(name);
    self->dirty = dirty;
    clear_groups(self->undo_groups);
    clear_groups(self->redo_groups);
    refresh_view(self);
}

static char *base_name_without_extension(GFile *file)
{
    g_autofree char *base = g_file_get_basename(file);
    char *dot = strrchr(base, '.');
    if (dot && dot != base)
        *dot = '\0';
    return g_strdup(base);
}

/* Languages */

static const SuzLanguageInfo *find_language(SuzWindow *self, const char *name)
{
    for (guint i = 0; i < self->languages->len; i++) {
        const SuzLanguageInfo *info = g_ptr_array_index(self->languages, i);
        if (g_str_equal(info->name, name))
            return info;
    }
    return NULL;
}

static char *state_path(void)
{
    return g_build_filename(g_get_user_config_dir(), "suz", "state.ini", NULL);
}

static char *load_saved_language(void)
{
    g_autofree char *path = state_path();
    g_autoptr(GKeyFile) keys = g_key_file_new();
    if (!g_key_file_load_from_file(keys, path, G_KEY_FILE_NONE, NULL))
        return NULL;
    return g_key_file_get_string(keys, "State", "language", NULL);
}

static void save_language(const char *name)
{
    g_autofree char *path = state_path();
    g_autofree char *dir = g_path_get_dirname(path);
    g_autoptr(GKeyFile) keys = g_key_file_new();
    g_key_file_load_from_file(keys, path, G_KEY_FILE_NONE, NULL);
    g_key_file_set_string(keys, "State", "language", name);
    g_mkdir_with_parents(dir, 0700);
    g_key_file_save_to_file(keys, path, NULL);
}

static gboolean load_language(SuzWindow *self, const char *name)
{
    const SuzLanguageInfo *info = find_language(self, name);
    if (!info)
        return FALSE;

    g_autoptr(GError) error = NULL;
    suz_language *language = suz_languages_load(info, &error);
    if (!language) {
        show_error(self, "Cannot load the language", error->message);
        return FALSE;
    }

    g_autofree char *kept_name = g_strdup(name);
    g_autofree char *file = g_path_get_basename(info->path);
    if (self->language)
        suz_language_free(self->language);
    self->language = language;
    g_free(self->language_name);
    self->language_name = g_steal_pointer(&kept_name);
    g_free(self->language_file);
    self->language_file = g_steal_pointer(&file);
    update_ui(self);
    return TRUE;
}

static void on_language_changed(GSimpleAction *action, GVariant *value, gpointer user_data)
{
    SuzWindow *self = SUZ_WINDOW(user_data);
    const char *name = g_variant_get_string(value, NULL);
    if (!load_language(self, name))
        return;
    g_simple_action_set_state(action, value);
    save_language(name);
}

static void save_learned_weights(SuzWindow *self)
{
    g_autofree char *path = user_language_path(self);
    if (!self->language || !path)
        return;

    g_autofree char *dir = g_path_get_dirname(path);
    g_autoptr(GError) error = NULL;
    char *text = suz_language_to_text(self->language);
    if (g_mkdir_with_parents(dir, 0700) != 0)
        show_error(self, "Cannot save the learned weights", g_strerror(errno));
    else if (!g_file_set_contents(path, text, -1, &error))
        show_error(self, "Cannot save the learned weights", error->message);
    suz_free(text);
    update_ui(self);
}

static gboolean language_has_tag(suz_language *language, const char *tag)
{
    if (!language)
        return FALSE;
    int count = suz_language_tag_count(language);
    for (int i = 0; i < count; i++) {
        char *known = suz_language_tag(language, i);
        gboolean same = g_str_equal(known, tag);
        suz_free(known);
        if (same)
            return TRUE;
    }
    return FALSE;
}

/* Editing */

static void append_field(GString *command, const char *text)
{
    g_string_append_c(command, '\t');
    for (const char *p = text; *p; p++) {
        switch (*p) {
        case '\\': g_string_append(command, "\\\\"); break;
        case '\t': g_string_append(command, "\\t"); break;
        case '\n': g_string_append(command, "\\n"); break;
        case '\r': g_string_append(command, "\\r"); break;
        default: g_string_append_c(command, *p);
        }
    }
}

static gboolean perform(SuzWindow *self, const char *command, gboolean learn)
{
    char *error = NULL;
    int status = learn && self->language
        ? suz_perform_learning(self->session, self->language, command, &error)
        : suz_perform(self->session, command, &error);
    if (status != 0) {
        show_error(self, "Cannot apply the change", error);
        suz_free(error);
        return FALSE;
    }

    push_group(self->undo_groups, 1);
    clear_groups(self->redo_groups);
    self->dirty = TRUE;
    refresh_view(self);
    if (learn && self->language)
        save_learned_weights(self);
    return TRUE;
}

static gboolean morph_id(SuzWindow *self, int s, int t, int m, uint64_t *id)
{
    return self->session && suz_morph_id(self->session, s, t, m, id) == 0;
}

suz_session *suz_window_get_session(SuzWindow *self)
{
    return self->session;
}

suz_language *suz_window_get_language(SuzWindow *self)
{
    return self->language;
}

gboolean suz_window_set_morph_form(SuzWindow *self, int segment, int token, int morph, const char *form)
{
    uint64_t id;
    if (!morph_id(self, segment, token, morph, &id))
        return FALSE;

    char *current = suz_morph_form(self->session, segment, token, morph);
    gboolean same = g_str_equal(current, form);
    suz_free(current);
    if (same)
        return TRUE;

    g_autofree char *number = g_strdup_printf("%" PRIu64, id);
    GString *command = g_string_new("set-form");
    append_field(command, number);
    append_field(command, form);
    gboolean ok = perform(self, command->str, FALSE);
    g_string_free(command, TRUE);
    return ok;
}

gboolean suz_window_set_morph_gloss(SuzWindow *self, int segment, int token, int morph, const char *gloss)
{
    uint64_t id;
    if (!morph_id(self, segment, token, morph, &id))
        return FALSE;

    g_autofree char *number = g_strdup_printf("%" PRIu64, id);
    GString *command = g_string_new("set-gloss");
    append_field(command, number);

    g_autofree char *text = gloss ? g_strstrip(g_strdup(gloss)) : NULL;
    if (!text || !*text) {
        append_field(command, "0");
    } else {
        g_auto(GStrv) parts = g_strsplit(text, ".", -1);
        guint first_tag = 0;
        const char *lexical = "";
        if (!language_has_tag(self->language, parts[0])) {
            lexical = parts[0];
            first_tag = 1;
        }
        guint tags = 0;
        for (guint i = first_tag; parts[i]; i++)
            if (*parts[i])
                tags++;

        g_autofree char *count = g_strdup_printf("%u", tags);
        append_field(command, "1");
        append_field(command, lexical);
        append_field(command, count);
        for (guint i = first_tag; parts[i]; i++)
            if (*parts[i])
                append_field(command, parts[i]);
    }

    gboolean ok = perform(self, command->str, TRUE);
    g_string_free(command, TRUE);
    return ok;
}

static void on_morph_activated(SuzDocumentView *view, int segment, int token, int morph, GtkWidget *button, gpointer user_data)
{
    (void)view;
    suz_morph_editor_popup(SUZ_WINDOW(user_data), button, segment, token, morph);
}

/* Actions */

static void action_annotate(GSimpleAction *action, GVariant *parameter, gpointer user_data)
{
    (void)action;
    (void)parameter;
    SuzWindow *self = SUZ_WINDOW(user_data);
    char *error = NULL;

    int splits = suz_suggest_segmentation(self->session, self->language, &error);
    if (splits < 0) {
        show_error(self, "Cannot split the words", error);
        suz_free(error);
        return;
    }
    int glosses = suz_suggest_glosses(self->session, self->language, &error);
    if (glosses < 0) {
        show_error(self, "Cannot suggest glosses", error);
        suz_free(error);
        return;
    }

    guint steps = (splits > 0) + (glosses > 0);
    if (steps == 0) {
        show_toast(self, "Nothing to annotate");
        return;
    }

    push_group(self->undo_groups, steps);
    clear_groups(self->redo_groups);
    self->dirty = TRUE;
    refresh_view(self);

    g_autofree char *message = g_strdup_printf("Split %d, glossed %d", splits, glosses);
    show_toast(self, message);
}

static void run_steps(SuzWindow *self, GArray *from, GArray *to, gboolean undo)
{
    if (!self->session || from->len == 0)
        return;

    guint steps = g_array_index(from, guint, from->len - 1);
    g_array_set_size(from, from->len - 1);

    for (guint i = 0; i < steps; i++) {
        char *error = NULL;
        int status = undo ? suz_undo(self->session, &error) : suz_redo(self->session, &error);
        if (status != 0) {
            show_error(self, undo ? "Cannot undo" : "Cannot redo", error);
            suz_free(error);
            break;
        }
    }
    push_group(to, steps);
    self->dirty = TRUE;
    refresh_view(self);
}

static void action_undo(GSimpleAction *action, GVariant *parameter, gpointer user_data)
{
    (void)action;
    (void)parameter;
    SuzWindow *self = SUZ_WINDOW(user_data);
    run_steps(self, self->undo_groups, self->redo_groups, TRUE);
}

static void action_redo(GSimpleAction *action, GVariant *parameter, gpointer user_data)
{
    (void)action;
    (void)parameter;
    SuzWindow *self = SUZ_WINDOW(user_data);
    run_steps(self, self->redo_groups, self->undo_groups, FALSE);
}

/* Files */

static void open_text(SuzWindow *self, GFile *file)
{
    g_autoptr(GError) error = NULL;
    g_autofree char *text = NULL;
    if (!g_file_load_contents(file, NULL, &text, NULL, NULL, &error)) {
        show_error(self, "Cannot open the file", error->message);
        return;
    }
    if (!g_utf8_validate(text, -1, NULL)) {
        show_error(self, "Cannot open the file", "the text is not valid UTF-8");
        return;
    }

    char *message = NULL;
    suz_session *session = self->language
        ? suz_session_from_text_with_language(self->language, text, &message)
        : suz_session_from_text(text, &message);
    if (!session) {
        show_error(self, "Cannot import the text", message);
        suz_free(message);
        return;
    }

    g_autofree char *name = base_name_without_extension(file);
    replace_session(self, session, NULL, name, TRUE);
}

static void open_document(SuzWindow *self, GFile *file)
{
    g_autoptr(GError) error = NULL;
    g_autofree char *text = NULL;
    if (!g_file_load_contents(file, NULL, &text, NULL, NULL, &error)) {
        show_error(self, "Cannot open the file", error->message);
        return;
    }

    char *message = NULL;
    suz_session *session = suz_session_from_snapshot(text, &message);
    if (!session) {
        show_error(self, "Cannot read the document", message);
        suz_free(message);
        return;
    }

    g_autofree char *name = base_name_without_extension(file);
    replace_session(self, session, file, name, FALSE);
}

typedef struct {
    SuzWindow *window;
    GFile *file;
    gboolean document;
} PendingOpen;

static void pending_open_free(PendingOpen *pending)
{
    g_clear_object(&pending->file);
    g_free(pending);
}

static void on_discard_response(GObject *source, GAsyncResult *result, gpointer user_data)
{
    PendingOpen *pending = user_data;
    const char *response = adw_alert_dialog_choose_finish(ADW_ALERT_DIALOG(source), result);
    if (g_str_equal(response, "discard")) {
        if (pending->document)
            open_document(pending->window, pending->file);
        else
            open_text(pending->window, pending->file);
    }
    pending_open_free(pending);
}

static void open_file_guarded(SuzWindow *self, GFile *file, gboolean document)
{
    if (!self->session || !self->dirty) {
        if (document)
            open_document(self, file);
        else
            open_text(self, file);
        return;
    }

    PendingOpen *pending = g_new0(PendingOpen, 1);
    pending->window = self;
    pending->file = g_object_ref(file);
    pending->document = document;

    AdwDialog *dialog = adw_alert_dialog_new("Discard Changes?", "The current document has unsaved changes.");
    adw_alert_dialog_add_responses(ADW_ALERT_DIALOG(dialog), "cancel", "Cancel", "discard", "Discard", NULL);
    adw_alert_dialog_set_response_appearance(ADW_ALERT_DIALOG(dialog), "discard", ADW_RESPONSE_DESTRUCTIVE);
    adw_alert_dialog_set_default_response(ADW_ALERT_DIALOG(dialog), "cancel");
    adw_alert_dialog_set_close_response(ADW_ALERT_DIALOG(dialog), "cancel");
    adw_alert_dialog_choose(ADW_ALERT_DIALOG(dialog), GTK_WIDGET(self), NULL, on_discard_response, pending);
}

void suz_window_open_file(SuzWindow *self, GFile *file)
{
    g_autofree char *base = g_file_get_basename(file);
    open_file_guarded(self, file, g_str_has_suffix(base, ".suz"));
}

static GListModel *make_filters(const char *name, const char *pattern, const char *mime)
{
    GtkFileFilter *filter = gtk_file_filter_new();
    gtk_file_filter_set_name(filter, name);
    if (pattern)
        gtk_file_filter_add_pattern(filter, pattern);
    if (mime)
        gtk_file_filter_add_mime_type(filter, mime);

    GListStore *filters = g_list_store_new(GTK_TYPE_FILE_FILTER);
    g_list_store_append(filters, filter);
    g_object_unref(filter);
    return G_LIST_MODEL(filters);
}

static gboolean is_dismissal(GError *error)
{
    return g_error_matches(error, GTK_DIALOG_ERROR, GTK_DIALOG_ERROR_DISMISSED)
        || g_error_matches(error, GTK_DIALOG_ERROR, GTK_DIALOG_ERROR_CANCELLED)
        || g_error_matches(error, G_IO_ERROR, G_IO_ERROR_CANCELLED);
}

static void on_open_text_done(GObject *source, GAsyncResult *result, gpointer user_data)
{
    SuzWindow *self = SUZ_WINDOW(user_data);
    g_autoptr(GError) error = NULL;
    g_autoptr(GFile) file = gtk_file_dialog_open_finish(GTK_FILE_DIALOG(source), result, &error);
    if (file)
        open_file_guarded(self, file, FALSE);
    else if (!is_dismissal(error))
        show_error(self, "Cannot open the file", error->message);
}

static void on_open_document_done(GObject *source, GAsyncResult *result, gpointer user_data)
{
    SuzWindow *self = SUZ_WINDOW(user_data);
    g_autoptr(GError) error = NULL;
    g_autoptr(GFile) file = gtk_file_dialog_open_finish(GTK_FILE_DIALOG(source), result, &error);
    if (file)
        open_file_guarded(self, file, TRUE);
    else if (!is_dismissal(error))
        show_error(self, "Cannot open the file", error->message);
}

static void action_open_text(GSimpleAction *action, GVariant *parameter, gpointer user_data)
{
    (void)action;
    (void)parameter;
    SuzWindow *self = SUZ_WINDOW(user_data);
    g_autoptr(GtkFileDialog) dialog = gtk_file_dialog_new();
    g_autoptr(GListModel) filters = make_filters("Text files", NULL, "text/plain");
    gtk_file_dialog_set_title(dialog, "Open Text");
    gtk_file_dialog_set_filters(dialog, filters);
    gtk_file_dialog_open(dialog, GTK_WINDOW(self), NULL, on_open_text_done, self);
}

static void action_open_document(GSimpleAction *action, GVariant *parameter, gpointer user_data)
{
    (void)action;
    (void)parameter;
    SuzWindow *self = SUZ_WINDOW(user_data);
    g_autoptr(GtkFileDialog) dialog = gtk_file_dialog_new();
    g_autoptr(GListModel) filters = make_filters("Suz documents", "*.suz", NULL);
    gtk_file_dialog_set_title(dialog, "Open Document");
    gtk_file_dialog_set_filters(dialog, filters);
    gtk_file_dialog_open(dialog, GTK_WINDOW(self), NULL, on_open_document_done, self);
}

static gboolean write_text(SuzWindow *self, GFile *file, const char *text, const char *what)
{
    g_autoptr(GError) error = NULL;
    if (!g_file_replace_contents(file, text, strlen(text), NULL, FALSE, G_FILE_CREATE_REPLACE_DESTINATION, NULL, NULL, &error)) {
        show_error(self, what, error->message);
        return FALSE;
    }
    return TRUE;
}

static void save_to(SuzWindow *self, GFile *file)
{
    char *snapshot = suz_snapshot(self->session);
    gboolean ok = write_text(self, file, snapshot, "Cannot save the document");
    suz_free(snapshot);
    if (!ok) {
        self->close_after_save = FALSE;
        return;
    }

    g_clear_object(&self->file);
    self->file = g_object_ref(file);
    g_free(self->document_name);
    self->document_name = base_name_without_extension(file);
    self->dirty = FALSE;
    update_ui(self);
    show_toast(self, "Document saved");

    if (self->close_after_save) {
        self->close_after_save = FALSE;
        self->close_confirmed = TRUE;
        gtk_window_close(GTK_WINDOW(self));
    }
}

static void on_save_as_done(GObject *source, GAsyncResult *result, gpointer user_data)
{
    SuzWindow *self = SUZ_WINDOW(user_data);
    g_autoptr(GError) error = NULL;
    g_autoptr(GFile) file = gtk_file_dialog_save_finish(GTK_FILE_DIALOG(source), result, &error);
    if (file) {
        save_to(self, file);
        return;
    }
    self->close_after_save = FALSE;
    if (!is_dismissal(error))
        show_error(self, "Cannot save the document", error->message);
}

static void save_as(SuzWindow *self)
{
    g_autoptr(GtkFileDialog) dialog = gtk_file_dialog_new();
    g_autoptr(GListModel) filters = make_filters("Suz documents", "*.suz", NULL);
    g_autofree char *initial = g_strdup_printf("%s.suz", self->document_name ? self->document_name : "Untitled");
    gtk_file_dialog_set_title(dialog, "Save Document");
    gtk_file_dialog_set_filters(dialog, filters);
    gtk_file_dialog_set_initial_name(dialog, initial);
    gtk_file_dialog_save(dialog, GTK_WINDOW(self), NULL, on_save_as_done, self);
}

static void action_save(GSimpleAction *action, GVariant *parameter, gpointer user_data)
{
    (void)action;
    (void)parameter;
    SuzWindow *self = SUZ_WINDOW(user_data);
    if (self->file)
        save_to(self, self->file);
    else
        save_as(self);
}

static void action_save_as(GSimpleAction *action, GVariant *parameter, gpointer user_data)
{
    (void)action;
    (void)parameter;
    save_as(SUZ_WINDOW(user_data));
}

typedef struct {
    SuzWindow *window;
    char *format;
} PendingExport;

static void pending_export_free(PendingExport *pending)
{
    g_free(pending->format);
    g_free(pending);
}

static void on_export_done(GObject *source, GAsyncResult *result, gpointer user_data)
{
    PendingExport *pending = user_data;
    SuzWindow *self = pending->window;
    g_autoptr(GError) error = NULL;
    g_autoptr(GFile) file = gtk_file_dialog_save_finish(GTK_FILE_DIALOG(source), result, &error);

    if (file && self->session) {
        char *text = NULL;
        if (g_str_equal(pending->format, "interlinear"))
            text = suz_render(self->session);
        else if (g_str_equal(pending->format, "toolbox"))
            text = suz_toolbox(self->session);
        else if (g_str_equal(pending->format, "conllu"))
            text = suz_conllu(self->session);
        else if (g_str_equal(pending->format, "json"))
            text = suz_json(self->session);
        else if (g_str_equal(pending->format, "latex"))
            text = suz_latex(self->session);
        else if (g_str_equal(pending->format, "html"))
            text = suz_html(self->session);

        if (text && write_text(self, file, text, "Cannot export the document"))
            show_toast(self, "Document exported");
        suz_free(text);
    } else if (!file && !is_dismissal(error)) {
        show_error(self, "Cannot export the document", error->message);
    }
    pending_export_free(pending);
}

static void action_export(GSimpleAction *action, GVariant *parameter, gpointer user_data)
{
    (void)action;
    SuzWindow *self = SUZ_WINDOW(user_data);
    const char *format = g_variant_get_string(parameter, NULL);

    const char *extension = "txt";
    const char *label = "Export";
    for (guint i = 0; i < G_N_ELEMENTS(export_formats); i++) {
        if (g_str_equal(export_formats[i][0], format)) {
            label = export_formats[i][1];
            extension = export_formats[i][2];
        }
    }

    PendingExport *pending = g_new0(PendingExport, 1);
    pending->window = self;
    pending->format = g_strdup(format);

    g_autoptr(GtkFileDialog) dialog = gtk_file_dialog_new();
    g_autofree char *title = g_strdup(label);
    char *ellipsis = strstr(title, "…");
    if (ellipsis)
        *ellipsis = '\0';
    g_autofree char *initial = g_strdup_printf("%s.%s", self->document_name ? self->document_name : "Untitled", extension);
    gtk_file_dialog_set_title(dialog, title);
    gtk_file_dialog_set_initial_name(dialog, initial);
    gtk_file_dialog_save(dialog, GTK_WINDOW(self), NULL, on_export_done, pending);
}

/* Closing and dropping */

static void on_close_response(GObject *source, GAsyncResult *result, gpointer user_data)
{
    SuzWindow *self = SUZ_WINDOW(user_data);
    const char *response = adw_alert_dialog_choose_finish(ADW_ALERT_DIALOG(source), result);

    if (g_str_equal(response, "discard")) {
        self->close_confirmed = TRUE;
        gtk_window_close(GTK_WINDOW(self));
    } else if (g_str_equal(response, "save")) {
        self->close_after_save = TRUE;
        if (self->file)
            save_to(self, self->file);
        else
            save_as(self);
    }
}

static gboolean suz_window_close_request(GtkWindow *window)
{
    SuzWindow *self = SUZ_WINDOW(window);
    if (!self->session || !self->dirty || self->close_confirmed)
        return GTK_WINDOW_CLASS(suz_window_parent_class)->close_request(window);

    AdwDialog *dialog = adw_alert_dialog_new("Save Changes?", "The document has unsaved changes. They are lost if you close without saving.");
    adw_alert_dialog_add_responses(ADW_ALERT_DIALOG(dialog), "cancel", "Cancel", "discard", "Discard", "save", "Save", NULL);
    adw_alert_dialog_set_response_appearance(ADW_ALERT_DIALOG(dialog), "discard", ADW_RESPONSE_DESTRUCTIVE);
    adw_alert_dialog_set_response_appearance(ADW_ALERT_DIALOG(dialog), "save", ADW_RESPONSE_SUGGESTED);
    adw_alert_dialog_set_default_response(ADW_ALERT_DIALOG(dialog), "save");
    adw_alert_dialog_set_close_response(ADW_ALERT_DIALOG(dialog), "cancel");
    adw_alert_dialog_choose(ADW_ALERT_DIALOG(dialog), GTK_WIDGET(self), NULL, on_close_response, self);
    return TRUE;
}

static gboolean on_drop(GtkDropTarget *target, const GValue *value, double x, double y, gpointer user_data)
{
    (void)target;
    (void)x;
    (void)y;
    if (!G_VALUE_HOLDS(value, G_TYPE_FILE))
        return FALSE;
    suz_window_open_file(SUZ_WINDOW(user_data), g_value_get_object(value));
    return TRUE;
}

/* Construction */

static GMenu *build_primary_menu(SuzWindow *self)
{
    GMenu *menu = g_menu_new();

    GMenu *languages = g_menu_new();
    g_menu_append_submenu(languages, "Language", G_MENU_MODEL(self->language_menu));
    g_menu_append_section(menu, NULL, G_MENU_MODEL(languages));
    g_object_unref(languages);

    GMenu *export_menu = g_menu_new();
    for (guint i = 0; i < G_N_ELEMENTS(export_formats); i++) {
        g_autofree char *action = g_strdup_printf("win.export::%s", export_formats[i][0]);
        g_menu_append(export_menu, export_formats[i][1], action);
    }
    GMenu *exports = g_menu_new();
    g_menu_append_submenu(exports, "Export", G_MENU_MODEL(export_menu));
    g_menu_append_section(menu, NULL, G_MENU_MODEL(exports));
    g_object_unref(export_menu);
    g_object_unref(exports);

    GMenu *document = g_menu_new();
    g_menu_append(document, "Undo", "win.undo");
    g_menu_append(document, "Redo", "win.redo");
    g_menu_append(document, "Save", "win.save");
    g_menu_append(document, "Save As…", "win.save-as");
    g_menu_append_section(menu, NULL, G_MENU_MODEL(document));
    g_object_unref(document);

    GMenu *help = g_menu_new();
    g_menu_append(help, "Keyboard Shortcuts", "app.shortcuts");
    g_menu_append(help, "About Suz", "app.about");
    g_menu_append_section(menu, NULL, G_MENU_MODEL(help));
    g_object_unref(help);
    return menu;
}

static GtkWidget *build_header(SuzWindow *self)
{
    GtkWidget *header = adw_header_bar_new();

    GMenu *open_menu = g_menu_new();
    g_menu_append(open_menu, "Open Text…", "win.open-text");
    g_menu_append(open_menu, "Open Document…", "win.open-document");
    GtkWidget *open = adw_split_button_new();
    adw_split_button_set_label(ADW_SPLIT_BUTTON(open), "Open");
    adw_split_button_set_menu_model(ADW_SPLIT_BUTTON(open), G_MENU_MODEL(open_menu));
    gtk_actionable_set_action_name(GTK_ACTIONABLE(open), "win.open-text");
    gtk_widget_set_tooltip_text(open, "Open a text file");
    adw_header_bar_pack_start(ADW_HEADER_BAR(header), open);
    g_object_unref(open_menu);

    GtkWidget *history = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
    gtk_widget_add_css_class(history, "linked");
    GtkWidget *undo = gtk_button_new_from_icon_name("edit-undo-symbolic");
    gtk_actionable_set_action_name(GTK_ACTIONABLE(undo), "win.undo");
    gtk_widget_set_tooltip_text(undo, "Undo");
    GtkWidget *redo = gtk_button_new_from_icon_name("edit-redo-symbolic");
    gtk_actionable_set_action_name(GTK_ACTIONABLE(redo), "win.redo");
    gtk_widget_set_tooltip_text(redo, "Redo");
    gtk_box_append(GTK_BOX(history), undo);
    gtk_box_append(GTK_BOX(history), redo);
    adw_header_bar_pack_start(ADW_HEADER_BAR(header), history);

    self->title = ADW_WINDOW_TITLE(adw_window_title_new("Suz", ""));
    adw_header_bar_set_title_widget(ADW_HEADER_BAR(header), GTK_WIDGET(self->title));

    GtkWidget *menu_button = gtk_menu_button_new();
    gtk_menu_button_set_icon_name(GTK_MENU_BUTTON(menu_button), "open-menu-symbolic");
    gtk_menu_button_set_primary(GTK_MENU_BUTTON(menu_button), TRUE);
    gtk_widget_set_tooltip_text(menu_button, "Main Menu");
    g_autoptr(GMenu) primary = build_primary_menu(self);
    gtk_menu_button_set_menu_model(GTK_MENU_BUTTON(menu_button), G_MENU_MODEL(primary));
    adw_header_bar_pack_end(ADW_HEADER_BAR(header), menu_button);

    GtkWidget *save = gtk_button_new_from_icon_name("document-save-symbolic");
    gtk_actionable_set_action_name(GTK_ACTIONABLE(save), "win.save");
    gtk_widget_set_tooltip_text(save, "Save the document");
    adw_header_bar_pack_end(ADW_HEADER_BAR(header), save);

    GtkWidget *annotate = gtk_button_new_with_label("Annotate");
    gtk_actionable_set_action_name(GTK_ACTIONABLE(annotate), "win.annotate");
    gtk_widget_add_css_class(annotate, "suggested-action");
    gtk_widget_set_tooltip_text(annotate, "Split the words and suggest glosses");
    adw_header_bar_pack_end(ADW_HEADER_BAR(header), annotate);

    AdwBreakpoint *narrow = adw_breakpoint_new(adw_breakpoint_condition_parse("max-width: 640sp"));
    GValue hidden = G_VALUE_INIT;
    g_value_init(&hidden, G_TYPE_BOOLEAN);
    g_value_set_boolean(&hidden, FALSE);
    adw_breakpoint_add_setter(narrow, G_OBJECT(history), "visible", &hidden);
    adw_breakpoint_add_setter(narrow, G_OBJECT(save), "visible", &hidden);
    adw_application_window_add_breakpoint(ADW_APPLICATION_WINDOW(self), narrow);

    return header;
}

static GtkWidget *build_empty_page(void)
{
    GtkWidget *page = adw_status_page_new();
    adw_status_page_set_icon_name(ADW_STATUS_PAGE(page), "dev.anmitali.Suz");
    adw_status_page_set_title(ADW_STATUS_PAGE(page), "Open a Text");
    adw_status_page_set_description(ADW_STATUS_PAGE(page), "Choose a text file, or drop one here, to start glossing.");

    GtkWidget *button = gtk_button_new_with_label("Open Text…");
    gtk_actionable_set_action_name(GTK_ACTIONABLE(button), "win.open-text");
    gtk_widget_add_css_class(button, "suggested-action");
    gtk_widget_add_css_class(button, "pill");
    gtk_widget_set_halign(button, GTK_ALIGN_CENTER);
    adw_status_page_set_child(ADW_STATUS_PAGE(page), button);
    return page;
}

static void select_initial_language(SuzWindow *self)
{
    if (self->languages->len == 0)
        return;

    g_autofree char *saved = load_saved_language();
    const SuzLanguageInfo *info = saved ? find_language(self, saved) : NULL;
    if (!info)
        info = find_language(self, "English");
    if (!info)
        info = g_ptr_array_index(self->languages, 0);
    g_action_group_change_action_state(G_ACTION_GROUP(self), "language", g_variant_new_string(info->name));
}

static void suz_window_init(SuzWindow *self)
{
    self->undo_groups = g_array_new(FALSE, FALSE, sizeof(guint));
    self->redo_groups = g_array_new(FALSE, FALSE, sizeof(guint));
    self->languages = suz_languages_discover();
    self->language_menu = g_menu_new();

    for (guint i = 0; i < self->languages->len; i++) {
        const SuzLanguageInfo *info = g_ptr_array_index(self->languages, i);
        GMenuItem *item = g_menu_item_new(info->name, NULL);
        g_menu_item_set_action_and_target(item, "win.language", "s", info->name);
        g_menu_append_item(self->language_menu, item);
        g_object_unref(item);
    }

    const GActionEntry entries[] = {
        {"open-text", action_open_text, NULL, NULL, NULL, {0}},
        {"open-document", action_open_document, NULL, NULL, NULL, {0}},
        {"save", action_save, NULL, NULL, NULL, {0}},
        {"save-as", action_save_as, NULL, NULL, NULL, {0}},
        {"export", action_export, "s", NULL, NULL, {0}},
        {"undo", action_undo, NULL, NULL, NULL, {0}},
        {"redo", action_redo, NULL, NULL, NULL, {0}},
        {"annotate", action_annotate, NULL, NULL, NULL, {0}},
        {"language", NULL, "s", "''", on_language_changed, {0}},
    };
    g_action_map_add_action_entries(G_ACTION_MAP(self), entries, G_N_ELEMENTS(entries), self);

    self->view = suz_document_view_new();
    g_signal_connect(self->view, "morph-activated", G_CALLBACK(on_morph_activated), self);

    self->stack = GTK_STACK(gtk_stack_new());
    gtk_stack_set_transition_type(self->stack, GTK_STACK_TRANSITION_TYPE_CROSSFADE);
    gtk_stack_add_named(self->stack, build_empty_page(), "empty");
    gtk_stack_add_named(self->stack, GTK_WIDGET(self->view), "document");

    GtkWidget *toolbar = adw_toolbar_view_new();
    adw_toolbar_view_add_top_bar(ADW_TOOLBAR_VIEW(toolbar), build_header(self));
    adw_toolbar_view_set_content(ADW_TOOLBAR_VIEW(toolbar), GTK_WIDGET(self->stack));

    self->toasts = ADW_TOAST_OVERLAY(adw_toast_overlay_new());
    adw_toast_overlay_set_child(self->toasts, toolbar);
    adw_application_window_set_content(ADW_APPLICATION_WINDOW(self), GTK_WIDGET(self->toasts));

    GtkDropTarget *drop = gtk_drop_target_new(G_TYPE_FILE, GDK_ACTION_COPY);
    g_signal_connect(drop, "drop", G_CALLBACK(on_drop), self);
    gtk_widget_add_controller(GTK_WIDGET(self->stack), GTK_EVENT_CONTROLLER(drop));

    gtk_widget_set_size_request(GTK_WIDGET(self), 360, 294);
    gtk_window_set_default_size(GTK_WINDOW(self), 960, 680);
    gtk_window_set_title(GTK_WINDOW(self), "Suz");

    select_initial_language(self);
    update_ui(self);
}

static void suz_window_finalize(GObject *object)
{
    SuzWindow *self = SUZ_WINDOW(object);
    if (self->session)
        suz_session_free(self->session);
    if (self->language)
        suz_language_free(self->language);
    g_clear_object(&self->file);
    g_clear_object(&self->language_menu);
    g_clear_pointer(&self->languages, g_ptr_array_unref);
    g_clear_pointer(&self->undo_groups, g_array_unref);
    g_clear_pointer(&self->redo_groups, g_array_unref);
    g_free(self->language_name);
    g_free(self->document_name);
    G_OBJECT_CLASS(suz_window_parent_class)->finalize(object);
}

static void suz_window_class_init(SuzWindowClass *klass)
{
    G_OBJECT_CLASS(klass)->finalize = suz_window_finalize;
    GTK_WINDOW_CLASS(klass)->close_request = suz_window_close_request;
}

SuzWindow *suz_window_new(AdwApplication *application)
{
    return g_object_new(SUZ_TYPE_WINDOW, "application", application, NULL);
}
