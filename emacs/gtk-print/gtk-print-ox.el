;;; gtk-print-ox.el --- org-mode print exporter -*- lexical-binding: t -*-

;;; Commentary:
;; Implements an injector function that adds a print menu-entry to ox-latex.

;;; Code:

(require 'ox)
(require 'ox-latex)

(require 'gtk-print)

(defcustom gtk-print-ox-print-settings-file
  (expand-file-name (locate-user-emacs-file "gtk-print-ox.printsettings"))
  "File to which `gtk-print-export-to-pdf-print' saves print settings.
Set to nil to not remember print dialog settings between exports."
  :group 'gtk-print-ox
  :type '(choice (file :tag "Save print settings to file")
                 (const :tag "Do not remember print settings." nil)))

(defun gtk-print-export-to-pdf-print (&rest args)
  "Export to PDF via latex and print.
ARGS are passed verbatim to `org-latex-export-to-pdf'"
  (gtk-print-file (apply #'org-latex-export-to-pdf args)
                   gtk-print-ox-print-settings-file))

(defun gtk-print-ox-inject ()
  "Inject a print option into org-mode's latex-export menu."
  (catch 'return
    (dolist (backend org-export-registered-backends)
      (when (string= (org-export-backend-name backend) "latex")
        ;; menu is a cons; destructively add a new entry to it.
        (nconc (nth 2 (org-export-backend-menu backend))
               '((?P "As PDF file and print" gtk-print-export-to-pdf-print)))
        (throw 'return nil)))))

(provide 'gtk-print-ox)
;;; gtk-print-ox.el ends here
