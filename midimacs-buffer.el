(eval-when-compile
  (require 'cl))
(require 'midimacs-globals)

(defun midimacs-code-open-window (code)
  (let* ((buffer-name (midimacs-buffer-code-name (midimacs-code-name code)))
         (buffer-existed (get-buffer buffer-name))
         (visible-window (get-buffer-window buffer-name))
         (buffer (get-buffer-create buffer-name)))

    (unless buffer-existed
      (with-current-buffer buffer
        (insert (midimacs-code-text code))
        (goto-char (point-min))
        (midimacs-code-mode)))

    (if visible-window
        (select-window visible-window)
      (when (one-window-p t)
        (split-window))
      (other-window 1))

    (switch-to-buffer buffer)))

(defun midimacs-set-buffer-seq ()
  (set-buffer (midimacs-buffer-seq-name)))

(defun midimacs-set-buffer-code (code)
  (set-buffer (midimacs-buffer-code-name (midimacs-code-name code))))

(defun midimacs-buffer-is-code (buffer)
  (string-match "^\\*midimacs-code-.\\*$" (buffer-name buffer)))

(defun midimacs-buffer-seq-name ()
  "*midimacs-seq*")

(defun midimacs-buffer-code-name (code-name)
  (concat "*midimacs-code-" (string code-name) "*"))

(defun midimacs-buffer-seq ()
  (get-buffer (midimacs-buffer-seq-name)))

(defun midimacs-buffer-code (code-name)
  (get-buffer (midimacs-buffer-code-name code-name)))

(defun midimacs-code-get-open-buffers ()
  (remove nil (loop for name being the hash-keys of midimacs-codes
                    collect (midimacs-buffer-code name))))

(defun midimacs-close-all-code-buffers ()
  (loop for buffer in (buffer-list)
        when (midimacs-buffer-is-code buffer)
        do (kill-buffer buffer)))

(defun midimacs-code-update-open-buffers ()
  (dolist (buffer (midimacs-code-get-open-buffers))
    (with-current-buffer buffer
      (midimacs-code-update))))

(defun midimacs-seq-buffer-contents ()
  (with-current-buffer (midimacs-buffer-seq)
    (save-excursion
      (goto-char (point-min))
      (end-of-line)
      (forward-char)
      (buffer-substring-no-properties (point) (point-max)))))


(provide 'midimacs-buffer)
