/**
 * MIT License
 *
 * Copyright (c) 2020 Nikita Bloshchanevich
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
#include <glib/gi18n.h> /* _, N_ */
#include <gtk/gtk.h>    /* gtk_init_with_args */
#include <locale.h>     /* setlocale */
#include <print.h>

#ifdef CONFIG_ENABLE_FORK
#include <sys/types.h> /* pid_t */
#include <unistd.h>    /* fork */
#endif

int main(int argc, char **argv) {
    setlocale(LC_ALL, "");
    bindtextdomain(GETTEXT_PACKAGE, DATADIR "/locale");
    bind_textdomain_codeset(GETTEXT_PACKAGE, "UTF-8");
    textdomain(GETTEXT_PACKAGE);

    static const char *password_input_method = "gui";
    static const char *password = NULL;
    static const char *print_settings_file = NULL;
    static const char *print_settings_output_file = NULL;
    static const char *action = "dialog";
#ifdef CONFIG_ENABLE_FORK
    static gboolean should_fork = FALSE;
#endif
    static const GOptionEntry options[] = {
        {
            "password-query-method", 'm', 0, G_OPTION_ARG_STRING,
            &password_input_method,
            N_("Specify how the password of an encrypted document will be queried. "
               "Defaults to gui. Possible values are:\n"
               "- 'none': do not query the password and fail if the file is encrypted\n"
               "- 'getpass': use the POSIX getpass() function to acquire the password\n"
               "- 'gui': get the password with a graphical dialog"),
            N_("method")
        },
        {
            "password", 'p', 0, G_OPTION_ARG_STRING, &password,
            N_("Set a password that is tried before prompting the user."),
            N_("password")
        },
        {
            "load-settings", 's', 0, G_OPTION_ARG_FILENAME,
            &print_settings_file,
            N_("Set the file from which the print settings will be loaded."),
            N_("file")
        },
        {
            "save-settings", 'S', 0, G_OPTION_ARG_FILENAME,
            &print_settings_output_file,
            N_("Set the file to which the print settings will be saved on a "
               "successful print."),
            N_("settings")
        },
        {
            "action", 'a', 0, G_OPTION_ARG_STRING, &action,
            N_("Specify what is to be done. Can be either 'print', 'preview' or 'dialog'"),
            N_("action")
        },
#ifdef CONFIG_ENABLE_FORK
        {
            "fork", 'F', 0, G_OPTION_ARG_NONE, &should_fork,
            N_("Fork and exit after opening the specified document"), NULL
        },
#endif
        { NULL }
    };

    GError *error = NULL;
    gboolean argparse_result = gtk_init_with_args(
        &argc, &argv, "document", options, GETTEXT_PACKAGE, &error);
    if (!argparse_result) {
        fprintf(stderr, _("error: parsing arguments failed: %s\n"),
                error->message);
        g_error_free(error);
        return 1;
    }

    PassQueryMethod pass_input;
    if (!method_from_name(&pass_input, password_input_method)) {
        fprintf(stderr, _("error: invalid password input method '%s'\n"),
                password_input_method);
        return 1;
    }

    const char *program_name = (argc >= 1) ? argv[0] : "gtk-print";
    if (argc != 2) {
        fprintf(stderr, _("usage: %s [options...] document\n"), program_name);
        return 1;
    }

    PopplerDocument *doc = open_document_interactively(argv[1], pass_input, password, &error);
    if (doc == NULL) {
        fprintf(stderr, _("error: failed to open document: %s\n"), error->message);
        g_error_free(error);
        return 2;
    }

#ifdef CONFIG_ENABLE_FORK
    if (should_fork) {
        pid_t pid = fork();

        if (pid < 0) {
            g_object_unref(doc);

            fputs(_("error: fork failed\n"), stderr);
            return 3;
        }

        if (pid != 0) /* Exit main thread, leaving only the child */
            return 0;
    }
#endif

    GtkPrintSettings *settings;
    if (print_settings_file != NULL) {
        settings = gtk_print_settings_new_from_file(print_settings_file, &error);
        if (settings == NULL) {
            g_object_unref(doc);

            fprintf(stderr, _("error: failed to load print settings: %s\n"),
                    error->message);
            g_error_free(error);
            return 4;
        }
    }
    else
        settings = gtk_print_settings_new();

    GtkPrintOperationAction print_action;
    if (!parse_print_action(&print_action, action)) {
        g_object_unref(doc);
        g_object_unref(settings);

        fprintf(stderr, _("error: failed to parse print action '%s'\n"), action);
        return 5;
    }

    GtkPrintOperationResult print_result =
        print_document(doc, &settings, print_action, &error);
    g_object_unref(doc);
    if (print_result == GTK_PRINT_OPERATION_RESULT_ERROR) {
        g_object_unref(settings);

        fprintf(stderr, _("error: printing failed: %s\n"), error->message);
        g_error_free(error);
        return 6;
    }

    if (print_result == GTK_PRINT_OPERATION_RESULT_APPLY)
        puts("apply");
    else
        puts("cancel");

    if (print_settings_output_file != NULL) {
        if (!gtk_print_settings_to_file(settings, print_settings_output_file,
                                        &error)) {
            g_object_unref(settings);

            fprintf(stderr, _("error: failed to save print settings: %s\n"),
                    error->message);
            g_error_free(error);
            return 7;
        }
    }
    g_object_unref(settings);

    return 0;
}
