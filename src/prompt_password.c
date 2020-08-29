#include <glib/gi18n.h>
#include <gtk/gtk.h>
#include <prompt_password.h>
#include <stdio.h>
#include <string.h>

bool method_from_name(PassQueryMethod *out, const char *name) {
    if (strcmp(name, "gui") == 0)
        *out = GUI;
#ifdef CONFIG_ENABLE_GETPASS
    else if (strcmp(name, "getpass") == 0)
        *out = GETPASS;
    else if (strcmp(name, "getpass-cli") == 0)
        *out = GETPASS_CLI;
#endif
    else if (strcmp(name, "none") == 0)
        *out = NONE;
    else
        return false;

    return true;
}

#ifdef CONFIG_USE_STRDUP
static char *dup_str(const char *string) {
#ifdef _WIN32
    return _strdup(string);
#else
    return strdup(string);
#endif
}
#else
static char *dup_str(const char *string) {
    size_t new_len = strlen(string) + 1; /* + nul byte */

    char *out = (char *)malloc(new_len);
    if (out != NULL)
        strncpy(out, string, new_len);

    return out;
}
#endif

static gboolean h_entry_return(GtkAccelGroup *accel_group_, GObject *o_dialog,
                               guint keyval_, GdkModifierType modifier_) {
    GtkDialog *dialog = GTK_DIALOG(o_dialog);
    gtk_dialog_response(dialog, 1);

    return TRUE;
}

char *gtk_prompt_password(const char *document) {
    GtkWidget *w_dialog =
        gtk_message_dialog_new(NULL, (GtkDialogFlags)0, GTK_MESSAGE_QUESTION,
                               GTK_BUTTONS_CANCEL, _("Password required"));
    GtkMessageDialog *dialog = GTK_MESSAGE_DIALOG(w_dialog);
    gtk_message_dialog_format_secondary_text(
        dialog,
        _("The document “%s“ is locked and requires a password before it "
          "can be printed."),
        document);
    gtk_dialog_add_button(GTK_DIALOG(dialog), _("Unlock"), 1);

    GtkLabel *password_label = GTK_LABEL(gtk_label_new(_("Password: ")));

    GtkEntry *password_entry = GTK_ENTRY(gtk_entry_new());
    gtk_entry_set_visibility(password_entry, FALSE);
    gtk_widget_set_hexpand(GTK_WIDGET(password_entry), TRUE);
    gtk_widget_grab_focus(GTK_WIDGET(password_entry));

    GtkAccelGroup *accels = gtk_accel_group_new();
    GClosure *ret_cb_closure =
        g_cclosure_new(G_CALLBACK(h_entry_return), NULL, NULL);
    gtk_accel_group_connect(accels, GDK_KEY_Return, (GdkModifierType)0,
                            (GtkAccelFlags)0, ret_cb_closure);
    gtk_window_add_accel_group(GTK_WINDOW(dialog), accels);
    g_object_unref(accels);

    GtkGrid *password_grid = GTK_GRID(gtk_grid_new());
    gtk_grid_attach(password_grid, GTK_WIDGET(password_label), 0, 0, 1, 1);
    gtk_grid_attach(password_grid, GTK_WIDGET(password_entry), 1, 0, 1, 1);
    gtk_widget_show_all(GTK_WIDGET(password_grid));

    GtkContainer *message_area =
        GTK_CONTAINER(gtk_message_dialog_get_message_area(dialog));
    gtk_container_add(message_area, GTK_WIDGET(password_grid));

    gint id = gtk_dialog_run(GTK_DIALOG(dialog));

    char *result = NULL;
    if (id == 1)
        result = dup_str(gtk_entry_get_text(password_entry));

    gtk_widget_destroy(GTK_WIDGET(dialog));

    return result;
}

char *document_prompt_password(PassQueryMethod m, const char *document) {
#ifdef CONFIG_ENABLE_GETPASS
    char *getpass(const char *); /* HACK: getpass declared here */
#endif

    switch (m) {
#ifdef CONFIG_ENABLE_GETPASS
    case GETPASS:
        return getpass(_("Document password: "));
    case GETPASS_CLI:
        return getpass("Password: ");
#endif
    case GUI:
        return gtk_prompt_password(document);
    case NONE:
        return NULL;
    }

    return NULL;
}

void free_password(PassQueryMethod m, char *password) {
    switch (m) {
#ifdef CONFIG_ENABLE_GETPASS
    case GETPASS: /* static buffer */
    case GETPASS_CLI:
#endif
    case NONE: /* no password */
        break;
    case GUI:
        free(password);
        break;
    }
}
