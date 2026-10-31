/* SPDX-License-Identifier: GPL-3.0-or-later */
#include "suz-languages.h"

#include <gio/gio.h>
#include <string.h>

void suz_language_info_free(SuzLanguageInfo *info)
{
    if (!info)
        return;
    g_free(info->name);
    g_free(info->path);
    g_free(info);
}

static char *read_name(const char *text)
{
    const char *line = text;
    while (line && *line) {
        const char *end = strchr(line, '\n');
        if (g_str_has_prefix(line, "language\t")) {
            const char *start = line + strlen("language\t");
            return end ? g_strndup(start, end - start) : g_strdup(start);
        }
        line = end ? end + 1 : NULL;
    }
    return NULL;
}

static void add_directory(GPtrArray *languages, GHashTable *seen, const char *directory)
{
    g_autoptr(GDir) dir = g_dir_open(directory, 0, NULL);
    if (!dir)
        return;

    const char *entry;
    while ((entry = g_dir_read_name(dir))) {
        if (!g_str_has_suffix(entry, ".language") || g_hash_table_contains(seen, entry))
            continue;

        g_autofree char *path = g_build_filename(directory, entry, NULL);
        g_autofree char *text = NULL;
        if (!g_file_get_contents(path, &text, NULL, NULL))
            continue;

        char *name = read_name(text);
        if (!name)
            continue;

        SuzLanguageInfo *info = g_new0(SuzLanguageInfo, 1);
        info->name = name;
        info->path = g_steal_pointer(&path);
        g_hash_table_add(seen, g_strdup(entry));
        g_ptr_array_add(languages, info);
    }
}

static int compare_names(gconstpointer a, gconstpointer b)
{
    const SuzLanguageInfo *x = *(SuzLanguageInfo *const *)a;
    const SuzLanguageInfo *y = *(SuzLanguageInfo *const *)b;
    return g_utf8_collate(x->name, y->name);
}

GPtrArray *suz_languages_discover(void)
{
    GPtrArray *languages = g_ptr_array_new_with_free_func((GDestroyNotify)suz_language_info_free);
    g_autoptr(GHashTable) seen = g_hash_table_new_full(g_str_hash, g_str_equal, g_free, NULL);

    const char *override = g_getenv("SUZ_LANGUAGES_DIR");
    if (override) {
        g_auto(GStrv) parts = g_strsplit(override, G_SEARCHPATH_SEPARATOR_S, -1);
        for (int i = 0; parts[i]; i++)
            add_directory(languages, seen, parts[i]);
    }

    g_autofree char *user = g_build_filename(g_get_user_data_dir(), "suz", "languages", NULL);
    add_directory(languages, seen, user);

    const char *const *system = g_get_system_data_dirs();
    for (int i = 0; system[i]; i++) {
        g_autofree char *dir = g_build_filename(system[i], "suz", "languages", NULL);
        add_directory(languages, seen, dir);
    }

#ifdef SUZ_LANGUAGES_DIR
    add_directory(languages, seen, SUZ_LANGUAGES_DIR);
#endif

    g_ptr_array_sort(languages, compare_names);
    return languages;
}

suz_language *suz_languages_load(const SuzLanguageInfo *info, GError **error)
{
    g_autofree char *text = NULL;
    if (!g_file_get_contents(info->path, &text, NULL, error))
        return NULL;

    char *message = NULL;
    suz_language *language = suz_language_from_text(text, &message);
    if (!language) {
        g_set_error(error, G_IO_ERROR, G_IO_ERROR_INVALID_DATA, "%s: %s", info->path, message ? message : "unreadable language");
        suz_free(message);
    }
    return language;
}
