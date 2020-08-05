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
#ifndef PROMPT_PASSWORD_H
#define PROMPT_PASSWORD_H

#include <stdbool.h>

typedef enum {
    NONE,    /* Don't ask the user for the password and just quit */
#ifdef CONFIG_ENABLE_GETPASS
    GETPASS, /* Acquire the password with getpass */
#endif
    GUI,     /* Show a graphical password input dialog */
} PassQueryMethod;

bool method_from_name(PassQueryMethod *out, const char *method_name);

char *gtk_prompt_password(const char *document);

char *document_prompt_password(PassQueryMethod m, const char *document);
void free_password(PassQueryMethod m, char *password);

#endif
