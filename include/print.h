#ifndef PRINT_H_INCLUDED
#define PRINT_H_INCLUDED

#include <glib.h>            /* GError */
#include <gtk/gtk.h>         /* GtkPrintOperation* */
#include <poppler.h>         /* PopplerDocument */
#include <prompt_password.h> /* PassQueryMethod */

GtkPrintOperationResult print_document(PopplerDocument *doc,
                                       GtkPrintSettings **settings,
                                       GtkPrintOperationAction action,
                                       GError **error);
PopplerDocument *open_document_interactively(const char *doc_path,
                                             PassQueryMethod m,
                                             const char *password,
                                             GError **result_error);

bool parse_print_action(GtkPrintOperationAction *action, const char *name);

#endif
