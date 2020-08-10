# About
'gtk-print' is a simple command-line utility that lets the user create a gtk-based
print dialog from their scripts.

# Usage
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
- 'none': Do not query the password, and fail if password if the file is
  encrypted and [password] is incorrect.
  
The initial settings for the print dialog can be loaded from a file. Likewise,
the user's settings may be written to a file. This can be used to remember the
user's print settings between invocations, improving the user experience. The
settings to do so are '\--load-settings=[path]' and '--save-settings=[save
path]'. Settings are only saved to [save path] if the user presses the "Print"
or "Preview" buttons. This can be overridden with '\--always-save-settings', in
which case the user's print settings are always saved.

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
