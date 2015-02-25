(eval-when-compile
  (require 'cl))
(require 'midimacs-globals)

(defun midimacs-get-code (code-name)
  (gethash code-name midimacs-codes))

(defun midimacs-maybe-create-code (code-name)
  (let ((code (midimacs-get-code code-name)))
  (unless code
    (setq code (make-midimacs-code :name code-name
                                   :text (midimacs-code-template)))
    (puthash code-name code midimacs-codes))
  code))  

(defun midimacs-code-template ()
"(midimacs-init (channel song-time length state)

  nil)

(midimacs-run (channel song-time rel-time state)

  state)
")

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
  (midimacs-buffer-get-code-name buffer))

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

(defun midimacs-update-all-code-texts ()
  (loop for code being the hash-values of midimacs-codes
        do (progn
             (midimacs-code-open-window code)
             (setf (midimacs-code-text code) (buffer-substring-no-properties (point-min) (point-max)))
             (other-window 1))))

(defun midimacs-buffer-get-code-name (buffer)
  (let* ((name (buffer-name buffer))
         (match (string-match "^\\*midimacs-code-\\(.\\)\\*$" name)))
    (if match
        (string-to-char (match-string 1 name))
      nil)))

(defun midimacs-buffer-get-code (buffer)
  (let ((code-name (midimacs-buffer-get-code-name buffer)))
    (if code-name
        (midimacs-get-code code-name)
      nil)))

(defun midimacs-current-buffer-code ()
  (midimacs-buffer-get-code (current-buffer)))

(defun midimacs-code-eval-buffer ()
  (interactive)
  (let ((code (midimacs-current-buffer-code)))
    (eval-buffer)
    (setf (midimacs-code-text code) (buffer-substring-no-properties (point-min) (point-max)))))

(defun midimacs-visible-codes ()
  (delete-duplicates
   (loop for track in midimacs-tracks
         append (loop for event in (midimacs-track-events track)
                      if event
                      collect (midimacs-event-code event)))
   :key 'midimacs-code-name))


(provide 'midimacs-code)
