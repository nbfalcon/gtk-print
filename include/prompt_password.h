#ifndef PROMPT_PASSWORD_H
#define PROMPT_PASSWORD_H

#include <stdbool.h>

typedef enum {
    NONE,    /* Don't ask the user for the password and just quit */
    GETPASS, /* Acquire the password with getpass */
    GUI,     /* Show a graphical password input dialog */
} PassQueryMethod;

bool method_from_name(PassQueryMethod *out, const char *method_name);

char *gtk_prompt_password(const char *document);

char *document_prompt_password(PassQueryMethod m, const char *document);
void free_password(PassQueryMethod m, char *password);

#endif
