#include "print.h"
#include "prompt_password.h"

#include <gtk/gtk.h>
#include <poppler.h>
#include <stdlib.h>

static void pdf_h_begin_print(GtkPrintOperation *op, GtkPrintContext *ctx_,
                              gpointer poppler_doc) {
    (void)ctx_;

    PopplerDocument *doc = (PopplerDocument *)poppler_doc;

    gtk_print_operation_set_n_pages(op, poppler_document_get_n_pages(doc));
}

static void pdf_h_draw_page(GtkPrintOperation *op_, GtkPrintContext *ctx,
                            gint page_nr, gpointer poppler_doc) {
    (void)op_;

    PopplerDocument *doc = (PopplerDocument *)poppler_doc;
    cairo_t *cr = gtk_print_context_get_cairo_context(ctx);

    PopplerPage *page = poppler_document_get_page(doc, page_nr);
    poppler_page_render_for_printing(page, cr);
    g_object_unref(page);
}

/**
 * @brief Opens the document specified by the GFile as a PopplerDocument. If it
 * is password protected and if @p password is invalid, the function will
 * prompt the user for it with the method specified by @p m
 * @param path Path to the file that is to be opened.
 * @param error If opening the document fails and it is not NULL, the error
 * will be stored in this variable.
 * @param m Method used to query the password.
 * @param password This password is tried first before asking the user. It may
 * be NULL.
 * @return If the document could be opened successfully, a PopplerDocument*
 * will be returned. It is a GObject and has one reference, and as such the
 * caller owns it.
 */
PopplerDocument *open_document_interactively(const char *doc_path,
                                             PassQueryMethod m,
                                             const char *password,
                                             GError **result_error) {
    GFile *in = g_file_new_for_path(doc_path);

    GError *err = NULL;
    PopplerDocument *doc =
        poppler_document_new_from_gfile(in, password, NULL, &err);

    while (doc == NULL && err->code == POPPLER_ERROR_ENCRYPTED) {
        char *pass = document_prompt_password(m, doc_path);
        if (pass == NULL)
            /* Error must not be freed if there is no password, as in that case
             * the error must be returned to the user
             */
            break;

        g_error_free(err);
        err = NULL;

        doc = poppler_document_new_from_gfile(in, pass, NULL, &err);

        free_password(m, pass);
    }

    g_object_unref(in);

    if (doc == NULL)
        g_propagate_error(result_error, err);

    return doc;
}

/**
 * Prints a PopplerDocument with a GtkPrintOperation.
 * @param doc A pointer to the PopplerDocument that is to be printed. This is
 * just a reference, and this function does not take ownership of it.
 * @param error If the print operation fails and error is not NULL, a GError
 * describing the failure will be written to (*error).
 * @param action Specifies what is to be done.
 * GTK_PRINT_OPERATION_ACTION_EXPORT is unsupported and will fail.
 * @return The result of the internal call to gtk_print_operation_run is
 * returned.
 */
GtkPrintOperationResult print_document(PopplerDocument *doc,
                                       GtkPrintSettings **settings,
                                       GtkPrintOperationAction action,
                                       GError **error) {
    GtkPrintOperation *print = gtk_print_operation_new();
    g_signal_connect(print, "begin-print", G_CALLBACK(pdf_h_begin_print), doc);
    g_signal_connect(print, "draw-page", G_CALLBACK(pdf_h_draw_page), doc);
    gtk_print_operation_set_print_settings(print, *settings);

    GtkPrintOperationResult print_result =
        gtk_print_operation_run(print, action, NULL, error);

    g_object_unref(*settings);
    (*settings) = (GtkPrintSettings *)g_object_ref(
        gtk_print_operation_get_print_settings(print));

    g_object_unref(print);

    return print_result;
}

int parse_print_action(GtkPrintOperationAction *action, const char *name) {
    if (strcmp(name, "print") == 0)
        *action = GTK_PRINT_OPERATION_ACTION_PRINT;
    else if (strcmp(name, "dialog") == 0)
        *action = GTK_PRINT_OPERATION_ACTION_PRINT_DIALOG;
    else if (strcmp(name, "preview") == 0)
        *action = GTK_PRINT_OPERATION_ACTION_PREVIEW;
    else
        return 0;

    return 1;
}
