# About
'gtk-print' is a simple command-line utility that lets the user create a gtk-based
print dialog from their scripts.

# Usage

## CLI
The program is invoked as `gtk-print [options...] document`. The program will
then execute the specified action on document.
Available actions (specified with the '\--action=[action]' or '-a' option) are:

- 'print': Print the document, without creating an interactive dialog. Options
  may be loaded from a file. The user may be asked for the document's password,
  if it is encrypted (see the section below).
- 'preview': Immediately launch the print preview on document, from which the
  user can usually cause the document to printed. As above, no print dialog will
  be visible, however the document's password may be queried.
- 'dialog': Probably the most useful option, it causes the creation of a native,
  gtk-based print dialog where the user can set various print settings and
  preview or print the document.

A password which is used for encrypted documents can be specified with the
'\--password=[password]' option. If it is correct, the file to be printed is
unlocked with it and the script procedes normally. If, however, that password is
incorrect, the user can be queried with one of three methods, which can be
specified with the '\--password-query-method=[method]' option:

- 'gui': Ask the user for their password in a graphical dialog.
- 'getpass': Read the password from the user by invoking the POSIX getpass
  routine. The user may enter their password in the same terminal in which this
  program was invoked. Note that this option can be disabled at compilation
  time.
- 'getpass-cli': Like 'getpass', but always prompt with the string "Password: ",
  regardless of locale or language settings.
- 'none': Do not query the password, and fail if password if the file is
  encrypted and [password] is incorrect.
  
The initial settings for the print dialog can be loaded from a file. Likewise,
the user's settings may be written to a file. This can be used to remember the
user's print settings between invocations, improving the user experience. The
settings for that is '\--settings-file=[settings-file]'. gtk-print will then
remember print settings in [settings-file]. It need not exist when gtk-print is
invoked. If you need more fine grained control, '\--load-settings' and
'\--save-settings' can be specified additionally, overriding [settings-file] for
their respective operations. Note that if '\--load-settings' is used, the file
specified by it must already exist. Print settings are only saved if the user
does not cancel the dialog, unless the '\--always-save-settings' option is
specified.

Lastly, there is the '\--fork' option, which causes the utility to fork itself
after opening the selected document and to exit the main thread immediately
thereafter. On linux, the selected documented can then safely be deleted, even
if it had not been printed yet. This way, the print dialog can linger for a long
time without blocking commands scheduled for execution after the invocation of
gtk-print. It is useful if the print result is uninteresting and if, for
example, a temporary file, that needs to be deleted afterwards, needs to be
printed and if asynchronous facilities are unavailable in runner's environment.
This option can be disabled at build time.

The script prints the action the user chose in the print dialog: 'cancel', if
the user pressed the cancel button in the print dialog, and 'apply' if they
pressed print or preview. The result is always 'apply' if an action other than
'dialog' is specified.

## Emacs (org-mode)
gtk-print may be used with org-mode. To do so, load
[emacs/gtk-print.el](emacs/gtk-print.el) or add [emacs/](emacs/) to your load
path and load [emacs/gtk-print-ox.el](emacs/gtk-print-ox.el) after that. You can
then add a printing menu entry to ox-latex by evaluating (gtk-print-ox-inject).
For example, you can add the following snippet to your init.el:
```elisp
(load-file "<path-to-gtk-print>/emacs/gtk-print.el")
(load-file "<path-to-gtk-print>/emacs/gtk-print-ox.el")
(gtk-print-ox-inject)
```
To remove the newly added exporter, evaluate:
```elisp
(gtk-print-ox-remove)
```

# Compiling
This project uses the meson build system. To compile, just run:
```console
$ meson build
$ ninja -C build
```
All options will be enabled. If you are on windows, you may want to disable fork
and getpass.
The list of available options is (they can be specified as '-Doption=somevalue'
on the meson command line):

- 'enable-fork': Enable the \--fork option.
- 'enable-getpass': Enable the getpass method for querying the document's
  password.
- 'use-strdup': Allow gtk-print to use POSIX' strdup routine (handled by _strdup
  on windows). Otherwise, a fallback implementation is provided.

# Translation
gtk-print is set up to handle translations with gettext. An example translation
for German is provided.

Happy scripting!
