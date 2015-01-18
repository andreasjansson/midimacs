(eval-when-compile
  (require 'cl))
(require 'midimacs-globals)

(defun midimacs-save-as (filename)
  (interactive "FWrite midimacs project: ")
  (setq midimacs-filename filename)
  (midimacs-save))

(defun midimacs-save ()
  (interactive)
  (midimacs-code-update-open-buffers)
  (let ((filename (or midimacs-filename
                      (read-file-name "Write midimacs project: "))))
    (with-temp-buffer
      (insert (midimacs-serialize-project))
      (write-file filename))
    (setq midimacs-filename filename)
    (message (concat "Wrote " filename))))

(defun midimacs-serialize-project ()
  (prin1-to-string (list "midimacs project"
                         "v1"
                         midimacs-codes
                         midimacs-repeat-start
                         midimacs-repeat-end
                         (midimacs-seq-buffer-contents))))

(defun midimacs-open (filename)
  (interactive "fFind midimacs project: ")
  (let ((s (with-temp-buffer
             (insert-file-contents filename)
             (buffer-string))))

    (midimacs-unserialize-project s))
  (setq midimacs-filename filename))

(defun midimacs-unserialize-project (s)
  (destructuring-bind (header
                       version
                       codes
                       repeat-start
                       repeat-end
                       seq-buffer-contents)
      (read s)

    (unless (equal header "midimacs project")
      (user-error "This doesn't appear to be a midimacs project"))
    (unless (equal version "v1")
      (user-error (concat "Unknown version: " version)))

    (midimacs-init)

    (setq midimacs-codes codes)
    (setq midimacs-repeat-start repeat-start)
    (setq midimacs-repeat-end repeat-end)
    (setq midimacs-song-time midimacs-repeat-start)
    (midimacs-draw seq-buffer-contents)))

(provide 'midimacs-save)
