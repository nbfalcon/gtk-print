#include "prompt_password.h"

#include "i18n.h"

#include <gtk/gtk.h> /* gtk_init_with_args */
#include <locale.h>  /* setlocale */
#include <print.h>

#ifdef CONFIG_ENABLE_FORK
#include <sys/types.h> /* pid_t */
#include <unistd.h>    /* fork */
#endif

int main(int argc, char **argv) {
#ifdef CONFIG_ENABLE_I18N
    setlocale(LC_ALL, "");
    bindtextdomain(GETTEXT_PACKAGE, LOCALEDIR);
    bind_textdomain_codeset(GETTEXT_PACKAGE, "UTF-8");
    textdomain(GETTEXT_PACKAGE);
#endif

    enum ErrorCodes {
        SUCCESS,
        ARGPASE_ERROR,
        DOCOPEN_FAILED,
        FORK_FAILED,
        PRINTING_FAILED,
        SAVE_PRINT_SETTINGS_FAILED,
    };

    static const char *password_input_method = NULL;
    static const char *password = NULL;
    static const char *base_settings_file = NULL;
    static const char *arg_load_settings_file = NULL;
    static const char *arg_save_settings_file = NULL;
    static gboolean always_save_settings = FALSE;
    static const char *action = NULL;
#ifdef CONFIG_ENABLE_FORK
    static gboolean should_fork = FALSE;
#endif
    static const GOptionEntry options[] = {
        {
            "password-query-method",
            'm',
            0,
            G_OPTION_ARG_STRING,
            &password_input_method,
            N_("Specify how the password of an encrypted document will be "
               "queried. "
               "Defaults to gui. Possible values are:\n"
               "- 'none': do not query the password and fail if the file is "
               "encrypted\n"
               "- 'getpass': use the POSIX getpass() function to acquire the "
               "password\n"
               "- 'getpass-cli': like 'getpass', but with a fixed prompt\n"
               "- 'gui': get the password with a graphical dialog"),
            N_("method"),
        },
        {
            "password",
            'p',
            0,
            G_OPTION_ARG_STRING,
            &password,
            N_("Set a password that is tried before prompting the user."),
            N_("password"),
        },
        {
            "settings-file",
            'P',
            0,
            G_OPTION_ARG_FILENAME,
            &base_settings_file,
            N_("Set the file to store print settings."),
            N_("base-settings-file"),
        },
        {
            "load-settings",
            's',
            0,
            G_OPTION_ARG_FILENAME,
            &arg_load_settings_file,
            N_("Set the file from which the print settings will be loaded."),
            N_("file"),
        },
        {
            "save-settings",
            'S',
            0,
            G_OPTION_ARG_FILENAME,
            &arg_save_settings_file,
            N_("Set the file to which the print settings will be saved on a "
               "successful print."),
            N_("settings"),
        },
        {
            "always-save-settings",
            'A',
            0,
            G_OPTION_ARG_NONE,
            &always_save_settings,
            N_("Save the print settings even if the user presses \"Cancel\"."),
            NULL,
        },
        {
            "action",
            'a',
            0,
            G_OPTION_ARG_STRING,
            &action,
            N_("Specify what is to be done. Can be either 'print', 'preview' "
               "or 'dialog'"),
            N_("action"),
        },
#ifdef CONFIG_ENABLE_FORK
        {
            "fork",
            'F',
            0,
            G_OPTION_ARG_NONE,
            &should_fork,
            N_("Fork and exit after opening the specified document."),
            NULL,
        },
#endif
        { NULL },
    };

    GError *error = NULL;
    gboolean argparse_result = gtk_init_with_args(
        &argc, &argv, "document", options, GETTEXT_PACKAGE, &error);
    if (!argparse_result) {
        fprintf(stderr, _("error: parsing arguments failed: %s\n"),
                error->message);
        g_error_free(error);
        return ARGPASE_ERROR;
    }

    PassQueryMethod pass_input = GUI;
    if (password_input_method != NULL
        && !method_from_name(&pass_input, password_input_method)) {
        fprintf(stderr, _("error: invalid password input method '%s'\n"),
                password_input_method);
        return ARGPASE_ERROR;
    }

    const char *program_name = (argc >= 1) ? argv[0] : "gtk-print";
    if (argc != 2) {
        fprintf(stderr, _("usage: %s [options...] document\n"), program_name);
        return ARGPASE_ERROR;
    }

    GtkPrintOperationAction print_action = GTK_PRINT_OPERATION_ACTION_PRINT_DIALOG;
    if (action != NULL && !parse_print_action(&print_action, action)) {
        fprintf(stderr, _("error: failed to parse print action '%s'\n"),
                action);
        return ARGPASE_ERROR;
    }

    PopplerDocument *doc =
        open_document_interactively(argv[1], pass_input, password, &error);
    if (doc == NULL) {
        fprintf(stderr, _("error: failed to open document: %s\n"),
                error->message);
        g_error_free(error);
        return DOCOPEN_FAILED;
    }

#ifdef CONFIG_ENABLE_FORK
    if (should_fork) {
        pid_t pid = fork();

        if (pid < 0) {
            g_object_unref(doc);

            fputs(_("error: fork failed\n"), stderr);
            return FORK_FAILED;
        }

        if (pid != 0) /* Exit main thread, leaving only the child */
            return 0;
    }
#endif

    /* If --settings-file is given, we must load from --load-settings only if
     * the file specified by --settings-file does not exist. */
    const char *const load_settings_file = (base_settings_file != NULL)
                                               ? base_settings_file
                                               : arg_load_settings_file;
    GtkPrintSettings *settings = NULL;
    if (load_settings_file != NULL) {
        settings =
            gtk_print_settings_new_from_file(load_settings_file, &error);
        if (settings == NULL) {
            /* The settings file not existing is normal if --settings-file is
             * specified. */
            if (base_settings_file != NULL
                && error->code == G_FILE_ERROR_NOENT) {
                g_error_free(error);
                error = NULL;

                /* If --load-settings is specified, we should load the print
                 * settings from --load-settings instead of using the default
                 * set obtained from gtk_print_settings_new(). */
                if (arg_load_settings_file != NULL) {
                    settings = gtk_print_settings_new_from_file(
                        arg_load_settings_file, &error);
                }
            }

            /* Either loading the settings yielded some strange error or
             * loading the fallback specified by --load-settings failed. */
            if (error != NULL) {
                g_object_unref(doc);

                fprintf(stderr,
                        _("error: failed to load print settings: %s\n"),
                        error->message);
                g_error_free(error);
                return 4;
            }
        }
    }
    /* No settings loading options specified at all or --settings-file failed
     * to load the settings, successfully */
    if (settings == NULL)
        settings = gtk_print_settings_new();

    GtkPrintOperationResult print_result =
        print_document(doc, &settings, print_action, &error);
    g_object_unref(doc);
    if (print_result == GTK_PRINT_OPERATION_RESULT_ERROR) {
        g_object_unref(settings);

        fprintf(stderr, _("error: printing failed: %s\n"), error->message);
        g_error_free(error);
        return PRINTING_FAILED;
    }

    puts((print_result == GTK_PRINT_OPERATION_RESULT_APPLY) ? "apply"
                                                            : "cancel");

    const char *const save_settings_file = (arg_save_settings_file != NULL)
                                               ? arg_save_settings_file
                                               : base_settings_file;
    if (save_settings_file != NULL
        && (always_save_settings
            || print_result == GTK_PRINT_OPERATION_RESULT_APPLY)) {
        if (!gtk_print_settings_to_file(settings, save_settings_file,
                                        &error)) {
            g_object_unref(settings);

            fprintf(stderr, _("error: failed to save print settings: %s\n"),
                    error->message);
            g_error_free(error);
            return SAVE_PRINT_SETTINGS_FAILED;
        }
    }
    g_object_unref(settings);

    return 0;
}
