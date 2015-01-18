(eval-when-compile
  (require 'cl))
(require 'midimacs-globals)

(defun midimacs-track-at-point ()
  (with-current-buffer (midimacs-buffer-seq)
    (save-excursion
      (beginning-of-line)
      (midimacs-parse-track (buffer-substring (point) (line-end-position))))))

(defun midimacs-beat-at-point ()
  (with-current-buffer (midimacs-buffer-seq)
    (midimacs-beat-at-column (current-column))))

(defun midimacs-code-at-point ()
  (let* ((track (midimacs-track-at-point))
         (beat (midimacs-beat-at-point))
         (code (when (and track beat) (midimacs-track-code-at-beat track beat))))
    code))

(defun midimacs-event-at-point ()
  (let* ((track (midimacs-track-at-point))
         (beat (midimacs-beat-at-point))
         (code (when (and track beat) (midimacs-track-event-at-beat track beat))))
    code))

(defun midimacs-track-event-at-beat (track beat)
  (elt (midimacs-track-events track) beat))

(defun midimacs-track-code-at-beat (track beat)
  (let ((event (midimacs-track-event-at-beat track beat)))
    (when event
      (midimacs-event-code event))))

(defun midimacs-track-events-at-beat (beat)
  (let ((event))
    (loop for track in midimacs-tracks
          if (setq event (midimacs-track-event-at-beat track beat))
          collect (list track event))))

(defun midimacs-event-rel-time (event time)
  (let ((event-time (midimacs-event-start-time event)))
    (midimacs-time- time event-time)))

(defun midimacs-event-length (event)
  (midimacs-time- (midimacs-event-end-time event)
                  (midimacs-event-start-time event)))

(defun midimacs-check-beat (beat)
  (unless (and beat (>= beat 0))
    (user-error "No beat here")))

(defun midimacs-get-code (code-name)
  (gethash code-name midimacs-codes))

(defun midimacs-beat-at-column (col)
  (when (>= col midimacs-left-bar-length)
    (- col midimacs-left-bar-length)))

(defun midimacs-event-code (track-event)
  (let ((code-name (midimacs-event-code-name track-event)))
    (midimacs-get-code code-name)))

(defun midimacs-sym-or-num-to-string (x)
  (cond ((stringp x) x)
        ((symbolp x) (symbol-name x))
        ((numberp x) (number-to-string x))))

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

(defun midimacs-make-scheduled-note-offs-heap ()
  (make-heap (lambda (a b) (midimacs-time< (nth 0 a) (nth 0 b)))))

(provide 'midimacs-util)

;; Local variables:
;; byte-compile-warnings: (not cl-functions)
;; End:
