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
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
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
