;;; gtk-print.el --- elisp interface for gtk-print -*- lexical-binding: t -*-

;;; Commentary:
;; Implements utilities to call gtk-print from elisp.

;;; Code:

(defcustom gtk-print-executable "gtk-print"
  "Location of the gtk-print program on your machine.
PATH lookup is performed on `gtk-print-executable'."
  :group 'gtk-print
  :type 'file)

(defun gtk-print-file (path settings-file)
  "Prints file PATH using gtk-print.
Load from and save print settings to SETTINGS-FILE. This function
does not block until the user finishes the dialog, however it may
block while querying the document's password from the user. PATH
may be deleted once this function returns."
  (apply #'call-process
         `(,gtk-print-executable
           nil nil nil "--fork"
           "--password-query-method=getpass"
           ,@(when settings-file
             (list (concat "--settings-file=" settings-file)))
           "--" ,path)))

(provide 'gtk-print)
;;; gtk-print.el ends here
