/* SPDX-License-Identifier: GPL-3.0-or-later */
#include <adwaita.h>

#include "suz-window.h"
#include "suz.h"

#define APPLICATION_ID "dev.anmitali.Suz"

#ifndef SUZ_GTK_VERSION
#define SUZ_GTK_VERSION "0.1.0"
#endif

static const char *const style =
    ".suz-surface { font-weight: 700; font-size: 1.1em; }\n"
    ".suz-form { font-style: italic; }\n"
    ".suz-morph { padding: 3px 6px; min-height: 0; min-width: 0; border-radius: 8px; }\n"
    ".suz-hyphen { opacity: 0.35; padding: 3px 0; }\n"
    ".suz-segment { padding: 18px 20px; }\n"
    ".suz-number { font-size: 0.85em; min-width: 1.4em; margin-top: 2px; }\n";

static void load_style(void)
{
    GtkCssProvider *provider = gtk_css_provider_new();
    gtk_css_provider_load_from_string(provider, style);
    gtk_style_context_add_provider_for_display(gdk_display_get_default(), GTK_STYLE_PROVIDER(provider),
                                               GTK_STYLE_PROVIDER_PRIORITY_APPLICATION);
    g_object_unref(provider);

#ifdef SUZ_ICONS_DIR
    gtk_icon_theme_add_search_path(gtk_icon_theme_get_for_display(gdk_display_get_default()), SUZ_ICONS_DIR);
#endif
}

static void action_about(GSimpleAction *action, GVariant *parameter, gpointer user_data)
{
    (void)action;
    (void)parameter;
    GtkApplication *application = GTK_APPLICATION(user_data);

    AdwDialog *about = adw_about_dialog_new();
    adw_about_dialog_set_application_name(ADW_ABOUT_DIALOG(about), "Suz");
    adw_about_dialog_set_application_icon(ADW_ABOUT_DIALOG(about), APPLICATION_ID);
    adw_about_dialog_set_version(ADW_ABOUT_DIALOG(about), SUZ_GTK_VERSION);
    adw_about_dialog_set_comments(ADW_ABOUT_DIALOG(about), "Morpheme-level interlinear glossing.");
    adw_about_dialog_set_license_type(ADW_ABOUT_DIALOG(about), GTK_LICENSE_GPL_3_0);
    adw_dialog_present(about, GTK_WIDGET(gtk_application_get_active_window(application)));
}

static void add_shortcut(AdwShortcutsSection *section, const char *title, const char *accelerator)
{
    adw_shortcuts_section_add(section, adw_shortcuts_item_new(title, accelerator));
}

static void action_shortcuts(GSimpleAction *action, GVariant *parameter, gpointer user_data)
{
    (void)action;
    (void)parameter;
    GtkApplication *application = GTK_APPLICATION(user_data);

    AdwDialog *dialog = adw_shortcuts_dialog_new();

    AdwShortcutsSection *documents = adw_shortcuts_section_new("Documents");
    add_shortcut(documents, "Open Text", "<Control>o");
    add_shortcut(documents, "Open Document", "<Control><Shift>o");
    add_shortcut(documents, "Save", "<Control>s");
    add_shortcut(documents, "Save As", "<Control><Shift>s");
    adw_shortcuts_dialog_add(ADW_SHORTCUTS_DIALOG(dialog), documents);

    AdwShortcutsSection *editing = adw_shortcuts_section_new("Editing");
    add_shortcut(editing, "Annotate", "<Control>Return");
    add_shortcut(editing, "Undo", "<Control>z");
    add_shortcut(editing, "Redo", "<Control><Shift>z");
    adw_shortcuts_dialog_add(ADW_SHORTCUTS_DIALOG(dialog), editing);

    AdwShortcutsSection *general = adw_shortcuts_section_new("General");
    add_shortcut(general, "Keyboard Shortcuts", "<Control>question");
    add_shortcut(general, "Quit", "<Control>q");
    adw_shortcuts_dialog_add(ADW_SHORTCUTS_DIALOG(dialog), general);

    adw_dialog_present(dialog, GTK_WIDGET(gtk_application_get_active_window(application)));
}

static void action_quit(GSimpleAction *action, GVariant *parameter, gpointer user_data)
{
    (void)action;
    (void)parameter;
    GtkApplication *application = GTK_APPLICATION(user_data);
    GList *windows = gtk_application_get_windows(application);
    for (GList *l = windows; l; l = l->next)
        gtk_window_close(GTK_WINDOW(l->data));
}

static void set_accel(GtkApplication *application, const char *action, const char *accelerator)
{
    const char *accels[] = {accelerator, NULL};
    gtk_application_set_accels_for_action(application, action, accels);
}

static void on_activate(GApplication *application, gpointer user_data)
{
    (void)user_data;
    GtkWindow *window = gtk_application_get_active_window(GTK_APPLICATION(application));
    if (!window)
        window = GTK_WINDOW(suz_window_new(ADW_APPLICATION(application)));
    gtk_window_present(window);
}

static void on_open(GApplication *application, GFile **files, int count, const char *hint, gpointer user_data)
{
    (void)hint;
    (void)user_data;
    for (int i = 0; i < count; i++) {
        GtkWindow *window = i == 0 ? gtk_application_get_active_window(GTK_APPLICATION(application)) : NULL;
        if (!window)
            window = GTK_WINDOW(suz_window_new(ADW_APPLICATION(application)));
        suz_window_open_file(SUZ_WINDOW(window), files[i]);
        gtk_window_present(window);
    }
}

static void on_startup_signal(GApplication *application, gpointer user_data)
{
    (void)user_data;
    GtkApplication *gtk_app = GTK_APPLICATION(application);

    load_style();
    gtk_window_set_default_icon_name(APPLICATION_ID);

    const GActionEntry entries[] = {
        {"about", action_about, NULL, NULL, NULL, {0}},
        {"shortcuts", action_shortcuts, NULL, NULL, NULL, {0}},
        {"quit", action_quit, NULL, NULL, NULL, {0}},
    };
    g_action_map_add_action_entries(G_ACTION_MAP(application), entries, G_N_ELEMENTS(entries), application);

    set_accel(gtk_app, "win.open-text", "<Control>o");
    set_accel(gtk_app, "win.open-document", "<Control><Shift>o");
    set_accel(gtk_app, "win.save", "<Control>s");
    set_accel(gtk_app, "win.save-as", "<Control><Shift>s");
    set_accel(gtk_app, "win.undo", "<Control>z");
    set_accel(gtk_app, "win.redo", "<Control><Shift>z");
    set_accel(gtk_app, "win.annotate", "<Control>Return");
    set_accel(gtk_app, "app.shortcuts", "<Control>question");
    set_accel(gtk_app, "app.quit", "<Control>q");
}

int main(int argc, char **argv)
{
    suz_init();
    g_set_application_name("Suz");

    AdwApplication *application = adw_application_new(APPLICATION_ID, G_APPLICATION_HANDLES_OPEN);
    g_signal_connect(application, "startup", G_CALLBACK(on_startup_signal), NULL);
    g_signal_connect(application, "activate", G_CALLBACK(on_activate), NULL);
    g_signal_connect(application, "open", G_CALLBACK(on_open), NULL);

    int status = g_application_run(G_APPLICATION(application), argc, argv);
    g_object_unref(application);
    suz_exit();
    return status;
}
