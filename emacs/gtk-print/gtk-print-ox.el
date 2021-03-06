;;; gtk-print-ox.el --- org-mode print exporter -*- lexical-binding: t -*-

;;; Commentary:
;; Implements an injector function that adds a print menu-entry to ox-latex.

;;; Code:

(require 'cl-lib)

(defcustom gtk-print-ox-print-settings-file
  (locate-user-emacs-file "gtk-print-ox.printsettings")
  "File to which `gtk-print-export-to-pdf-print' saves print settings.
Set to nil to not remember print dialog settings between exports."
  :group 'gtk-print-ox
  :type '(choice (file :tag "Save print settings to file")
                 (const :tag "Do not remember print settings." nil)))

;;;###autoload
(defun gtk-print-ox-clear-settings ()
  "Delete the print settings file, forgetting all previous settings."
  (interactive)
  (delete-file gtk-print-ox-print-settings-file))

(defun gtk-print-ox-export-to-pdf-print (&rest args)
  "`gtk-print' based org export function.
Internally, it first exports the current org file via
`org-latex-export-to-pdf', and then prints it. ARGS are passed
verbatim to `org-latex-export-to-pdf'."
  (require 'gtk-print)
  (declare-function gtk-print-file-async "gtk-print"
                    (f finish-cb &optional settings-f))
  (declare-function gtk-print-file-async-message-cb "gtk-print" (arg))
  (require 'ox-latex)
  (declare-function org-latex-export-to-pdf "ox-latex")
  (gtk-print-file-async (apply #'org-latex-export-to-pdf args)
                        #'gtk-print-file-async-message-cb
                        gtk-print-ox-print-settings-file))

(defconst gtk-print-ox--menuentry
  '((?P "As PDF file and print" gtk-print-ox-export-to-pdf-print))
  "The entry that is injected into the latex export menu.
It a list of entries to make it usable with `nconc'.")

(defun gtk-print-ox-latex-exporter-p (backend)
  "Return t if BACKEND is an ox-latex exporter, and nil otherwise.
BACKEND must be an instance of struct `org-export-backend'.

`ox' must already be loaded when this functino is called."
  (declare-function org-export-backend-name "ox")
  (string= (org-export-backend-name backend) "latex"))

(defun gtk-print-ox-find-latex-exporter ()
  "Find the `ox-latex' backend.
`ox' must already be loaded when this is called."
  (defvar org-export-registered-backends)
  (cl-find-if #'gtk-print-ox-latex-exporter-p org-export-registered-backends))

(defun gtk-print-ox-inject ()
  "Inject a print option into org-mode's latex-export menu.
Return t the option could be injected, and nil otherwise.
This function can be called multiple times. However, be wary that
problems could arise if called interleaved with similar injector
functions from other plugins. To undo the effects of this
function, call `gtk-print-ox-remove'."
  ;; don't break non-lazy configs
  (require 'ox)
  (declare-function org-export-backend-menu "ox")
  (require 'ox-latex)
  (when-let ((latex-exporter (gtk-print-ox-find-latex-exporter)))
    (nconc (nth 2 (org-export-backend-menu latex-exporter))
           gtk-print-ox--menuentry)
    ;; prevent looping if the user accidentally executes this function
    ;; twice.
    (when (eq (cdr gtk-print-ox--menuentry) gtk-print-ox--menuentry)
      (setcdr gtk-print-ox--menuentry nil))
    ;; return t: the latex exporter was found and injection was successful
    t))

(defun gtk-print-ox-remove ()
  "Undo the effects of `gtk-print-ox-inject'.
This function is safe to call even if `gtk-print-ox-inject' was
not called before. Return t if the ox-latex exporter was found,
and nil otherwise."
  (when-let ((latex-exporter (gtk-print-ox-find-latex-exporter))
             (menu (cddr (org-export-backend-menu latex-exporter))))
    (setcar menu (delq (car gtk-print-ox--menuentry) (car menu)))
    ;; return t: the latex exporter was found
    t))

;;;###autoload
(define-minor-mode gtk-print-ox-mode
  "Wraps `gtk-print-ox-inject' and `gtk-print-ox-remove'.
You should use this instead of `gtk-print-ox-inject' and
`gtk-print-ox-remove' directly, as using minor modes for
controlling Boolean state is more idiomatic."
  :global t
  :init-value nil
  :group 'gtk-print-ox
  (if gtk-print-ox-mode
      (gtk-print-ox-inject)
    (gtk-print-ox-remove)))

(provide 'gtk-print-ox)
;;; gtk-print-ox.el ends here
