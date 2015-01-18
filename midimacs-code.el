(eval-when-compile
  (require 'cl))
(require 'midimacs-globals)

(defun midimacs-get-code (code-name)
  (gethash code-name midimacs-codes))

(defun midimacs-maybe-create-code (code-name)
  (let ((code (midimacs-get-code code-name)))
  (unless code
    (setq code (make-midimacs-code :name code-name
                                   :text (midimacs-code-template code-name)))
    (puthash code-name code midimacs-codes))
  code))  

(defun midimacs-code-template (code-name)
  (concat
   "(midimacs-code
 ?" (string code-name) "

 ;; init
 (lambda (channel song-time length state)

   nil)

 ;; run
 (lambda (channel song-time rel-time state)

   state)

 )
"))

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

(defun midimacs-set-buffer-code (code)
  (set-buffer (midimacs-buffer-code-name (midimacs-code-name code))))

(defun midimacs-buffer-is-code (buffer)
  (string-match "^\\*midimacs-code-.\\*$" (buffer-name buffer)))

(defun midimacs-buffer-code-name (code-name)
  (concat "*midimacs-code-" (string code-name) "*"))

(defun midimacs-buffer-code (code-name)
  (get-buffer (midimacs-buffer-code-name code-name)))

(defun midimacs-code-get-open-buffers ()
  (remove nil (loop for name being the hash-keys of midimacs-codes
                    collect (midimacs-buffer-code name))))

(defun midimacs-close-all-code-buffers ()
  (loop for buffer in (buffer-list)
        when (midimacs-buffer-is-code buffer)
        do (kill-buffer buffer)))

(defun midimacs-eval-all-codes ()
  (loop for code being the hash-values of midimacs-codes
        do (progn
             (midimacs-code-open-window code)
             (eval-buffer)
             (other-window 1))))

(provide 'midimacs-code)
