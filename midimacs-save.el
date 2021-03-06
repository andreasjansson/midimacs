(eval-when-compile
  (require 'cl))
(require 'midimacs-globals)

(defun midimacs-save-as (filename)
  (interactive "FWrite midimacs project: ")
  (setq midimacs-filename filename)
  (midimacs-save))

(defun midimacs-save ()
  (interactive)
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
                         (midimacs-serialize-codes (midimacs-visible-codes))
                         midimacs-repeat-start
                         midimacs-repeat-end
                         (midimacs-seq-buffer-contents))))

(defun midimacs-serialize-codes (codes)
  (loop for code in codes
        for name = (midimacs-code-name code)
        for text = (midimacs-code-text code)
        unless (equal (string-trim text) (string-trim (midimacs-code-template)))
        collect (cons name text)))

(defun midimacs-open (filename)
  (interactive "fFind midimacs project: ")
  (let ((s (with-temp-buffer
             (insert-file-contents filename)
             (buffer-string))))

    (if (string-prefix-p "(\"midimacs project\"" s)
        (progn
          (midimacs-stop)
          (midimacs-unserialize-project s)
          (setq midimacs-filename filename))
      (find-file filename))))

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

    (midimacs-initialize)

    (setq midimacs-codes (midimacs-unserialize-codes codes))
    (midimacs-eval-all-codes)
    (setq midimacs-repeat-start repeat-start)
    (setq midimacs-repeat-end repeat-end)
    (setq midimacs-song-time midimacs-repeat-start)
    (midimacs-draw seq-buffer-contents)))

(defun midimacs-unserialize-codes (raw-codes)
  (let* ((codes (make-hash-table))
         (codes-list (loop for (name . text) in raw-codes
                           for code = (make-midimacs-code :name name :text text)
                           do (puthash name code codes))))
    codes))

(defun midimacs-seq-buffer-contents ()
  (with-current-buffer (midimacs-buffer-seq)
    (save-excursion
      (goto-char (point-min))
      (end-of-line)
      (forward-char)
      (buffer-substring-no-properties (point) (point-max)))))

(provide 'midimacs-save)
