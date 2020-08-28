;;; gtk-print.el --- elisp interface for gtk-print -*- lexical-binding: t -*-

;;; Commentary:
;; Implements utilities to call gtk-print from elisp.

;;; Code:

(defcustom gtk-print-command "gtk-print"
  "Location of the gtk-print program on your machine.
PATH lookup is performed on `gtk-print-command'."
  :type 'string
  :group 'gtk-print)

(defun gtk-print--file-shell-command (file &optional settings-file disable-fork)
  "Build a gtk-print shell command.
FILE is the file to print and SETTINGS-FILE is the file to store
print settings in. By default, the generated command specifies
--fork. To inhibit that behaviour, set DISABLE-FORK to t."
  (format "%s%s --password-query-method=none%s -- %s"
          (shell-quote-argument gtk-print-command)
          (if (or disable-fork (eq system-type 'windows-nt)) "" " --fork")
          (if settings-file
              (concat " --settings-file="
                      (shell-quote-argument (expand-file-name settings-file)))
            "")
          file))

;;;###autoload
(defun gtk-print-file (file &optional settings-file)
  "Prints file PATH using gtk-print.
Load from and save print settings to SETTINGS-FILE. This function
will block until FILE can be deleted, which is until the file has
been opened on UNIX-like platforms and until the print dialog
exits on Windows, so you should probably use
`gtk-print-file-async' instead. This is because Windows, unlike
Linux, does not allow deleting open files.

This family of functions currently has the limitation that it
cannot handle encrypted files, which will just fail."
  (interactive (list (read-file-name "Print file: ")))
  (call-process-shell-command
   (gtk-print--file-shell-command file settings-file)))

;;;###autoload
(defun gtk-print-file-async (file finish-callback &optional settings-file)
  "Like `gtk-print-file', but non-blocking.
After gtk-print finishes, call FINISH-CALLBACK accordingly.
FINISH-CALLBACK takes one argument: a status. It can be either
:accept or :cancel, representing the user's print dialog choice.
The second argument may be nil if gtk-print prints a strange
status. The semantics of FILE and SETTINGS-FILE are the same."
  (interactive (list (read-file-name "Print file: ")
                     (lambda (status) (pcase status
                                        (:apply (message "apply"))
                                        (:cancel (message "cancel"))
                                        (_ nil)))))
  (set-process-sentinel
   (start-process-shell-command
    "gtk-print"
    (generate-new-buffer " *gtk-print*")
    (gtk-print--file-shell-command file settings-file t))
   (lambda (proc ev)
     (when (string= ev "finished\n")
       (with-current-buffer (process-buffer proc)
         (funcall finish-callback
                  (pcase (buffer-string)
                    ("apply\n" :accept)
                    ("cancel\n" :cancel)
                    (_ nil)))
         (kill-buffer))))))

(defun gtk-print-tempfile (file &optional settings-file)
  "Like `gtk-print-file-async', but delete FILE afterwards.
FILE and SETTINGS-FILE are the same as in
`gtk-print-file-async'."
  (gtk-print-file-async file (lambda (_) (delete-file file)) settings-file))

(defcustom gtk-print-buffer-wkhtmltopdf-settings-file
  (locate-user-emacs-file "gtk-print-buffer-wkhtmltopdf.printsettings")
  "Print settings file of `gtk-print-buffer-wkhtmltopdf'.
Akin to `gtk-print-ox-print-settings-file'."
  :type 'file
  :group 'gtk-print-wkhtmltopdf)

(defcustom gtk-print-buffer-wkhtmltopdf-command "wkhtmltopdf"
  "Shell command to run wkhtmltopdf (https://wkhtmltopdf.org/).
It need not be quoted and may contain spaces and the like."
  :type 'string
  :group 'gtk-print-wkhtmltopdf)

;;;###autoload
(defun gtk-print-buffer-wkhtmltopdf (&optional settings-file)
  "Print the current buffer with gtk-print.
SETTINGS-FILE specifies a file that is used to store and load
settings from, or nil to not remember settings. This function
leverages `htmlize-buffer' and wkhtmltopdf to render the current
buffer to a PDF. Doing so has the following limitation: if Emacs
is configured with a dark theme, the buffer will be rendered with
a dark background, but with white borders."
  (interactive (list gtk-print-buffer-wkhtmltopdf-settings-file))
  (declare-function htmlize-buffer "htmlize" (&optional buffer))
  (require 'htmlize)
  (let ((html-result-buffer)
        (pdf-tempfile
         (shell-quote-argument
          (concat (make-temp-name (temporary-file-directory)) ".pdf"))))
    ;; BUG: if the user quits before the setq happens, the buffer created by
    ;; `htmlize-buffer' will not be killed. There is no way to fix this:
    ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Cleanups.html.
    (unwind-protect
        (progn
          (setq html-result-buffer (htmlize-buffer))
          (with-current-buffer html-result-buffer
            (call-shell-region
             (point-min) (point-max)
             (format
              ;; --fork is unnecessary here, as `call-shell-region' will not
              ;; block due to BUFFER being passed as 0.
              "%s - %s && (%s --password-query-method=none%s -- %s ; rm %s)"
              (shell-quote-argument gtk-print-buffer-wkhtmltopdf-command)
              pdf-tempfile
              (shell-quote-argument gtk-print-command)
              (or (and settings-file
                       (concat " --settings-file="
                               (shell-quote-argument
                                (expand-file-name settings-file)))) "")
              (shell-quote-argument pdf-tempfile)
              pdf-tempfile)
             nil 0)))
      (kill-buffer html-result-buffer))))

(provide 'gtk-print)
;;; gtk-print.el ends here
