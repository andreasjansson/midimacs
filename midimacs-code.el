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

(defun midimacs-rename-code (c)
  (interactive "cRename to: ")
  (when (midimacs-get-code c)
    (user-error "Code already exists!"))
  (let ((code (midimacs-code-at-point)))
    (unless code
      (user-error "No code at point!"))
    (setf (midimacs-code-name code) c)
    (puthash c code midimacs-codes)
    (loop for p in (midimacs-code-points code)
          do (delete-char 1)
          do (insert (string c)))))

(defun midimacs-code-points (code)
  (save-excursion
    (goto-char (point-min))
    (loop while (< (point) (point-max))
          for point = (point)
          for line = (buffer-substring-no-properties (line-beginning-position) (line-end-position))
          for c = (char-after)
          for is-col-0 = (= (current-column) 0)
          for is-track = (midimacs-string-is-track line)

          if (and is-col-0 (not is-track))
          do (forward-line 1)

          if (and is-col-0 is-track)
          do (forward-char 3)

          if (and (not is-col-0) is-track (eq c (midimacs-code-name code)))
          collect point

          if (not is-col-0)
          do (forward-char))))

(loop for x below 10
      if (> x 5)
      collect x
      else if (> x 4)
      collect "a"
      do (print x))


(provide 'midimacs-code)
