#ifndef PRINT_H_INCLUDED
#define PRINT_H_INCLUDED

#include <prompt_password.h> /* PassQueryMethod */
#include <poppler.h> /* PopplerDocument */
#include <gtk/gtk.h> /* GtkPrintOperation* */
#include <glib.h> /* GError */

GtkPrintOperationResult print_document(PopplerDocument *doc,
                                       GtkPrintSettings **settings,
                                       GtkPrintOperationAction action,
                                       GError **error);
PopplerDocument *open_document_interactively(const char *in, PassQueryMethod m,
                                             const char *password,
                                             GError **error);

bool parse_print_action(GtkPrintOperationAction * action, const char *name);

#endif
