#ifndef PROMPT_PASSWORD_H
#define PROMPT_PASSWORD_H

typedef enum {
    NONE, /* Don't ask the user for the password and just quit */
#ifdef CONFIG_ENABLE_GETPASS
    GETPASS, /* Acquire the password with getpass */
    /* Like GETPASS, but prompt with "Password: ", regardless of locale */
    GETPASS_CLI,
#endif
    GUI, /* Show a graphical password input dialog */
} PassQueryMethod;

int method_from_name(PassQueryMethod *out, const char *method_name);

char *gtk_prompt_password(const char *document);

char *document_prompt_password(PassQueryMethod m, const char *document);
void free_password(PassQueryMethod m, char *password);

#endif
