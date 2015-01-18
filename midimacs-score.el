(eval-when-compile
  (require 'cl))
(require 'midimacs-globals)
(require 'midimacs-time)
(require 'midimacs-util)

(defun midimacs-quantized-song-time ()
  (let ((seconds-per-tick (/ 60 (* midimacs-ticks-per-beat midimacs-bpm)))
        (seconds-since-tick (- (float-time) midimacs-last-tick-seconds)))
    (if (> seconds-since-tick (/ seconds-per-tick 2))
        (midimacs-time+ midimacs-song-time (make-midimacs-time :tick 1))
      midimacs-song-time)))

(defun midimacs-add-note-to-score (score time pitch duration)
  (midimacs-score-add-note score time pitch duration)
  (midimacs-score-update-buffer score))

(defun midimacs-remove-note-from-score (score time)
  (midimacs-score-remove-note score time)
  (midimacs-score-update-buffer score))

(defun midimacs-score-update-buffer (score)
  (with-current-buffer (midimacs-score-buffer score)
    (let ((p (search-backward "(midimacs-score" nil t)))
      (unless p
        (user-error "No midimacs-score in this buffer"))
      (beginning-of-line)
      (setq p (point))
      (forward-sexp)
      (delete-region p (point))
      (insert (midimacs-score-text score)))))

(defun midimacs-score-add-note (score time pitch duration)
  (let ((notes (midimacs-score-notes score)))
    (setf (midimacs-score-notes score)
          (loop for (tm p d) being the elements of notes using (index i) 
                while (midimacs-time>= time tm)
                finally (return (nconc (subseq notes 0 i)
                                       (list (list time pitch duration))
                                       (subseq notes i)))))))

(defun midimacs-score-remove-note (score time)
  (let ((notes (midimacs-score-notes score)))
    (setf (midimacs-score-notes score)
          (loop for (tm p d) being the elements of notes using (index i) 
                while (not (midimacs-time= time tm))
                finally (return (nconc (subseq notes 0 i)
                                       (subseq notes (1+ i))))))))

(cl-defun midimacs-score-text (score &key (hide-times nil))
  (let ((notes (if hide-times
                   (midimacs-score-notes-with-pauses score)
                 (midimacs-score-notes-without-pauses score))))
    (concat "   (midimacs-score\n"
            "    ("
            (apply 'concat (loop for (tm p d) being the elements of notes using (index i)
                                 collect (concat (if (= i 0) "(" "\n     (")
                                                 (if hide-times
                                                     (format "%-3s %s"
                                                             (midimacs-pitch-to-string p)
                                                             (midimacs-time-to-string d))
                                                   (format "%-9s %-3s %s"
                                                           (midimacs-time-to-string tm)
                                                           (midimacs-pitch-to-string p)
                                                           (midimacs-time-to-string d)))
                                                 ")")))
            "))")))

(defun midimacs-score-notes-without-pauses (score)
  (loop for (time pitch duration) in (midimacs-score-notes score)
        if pitch
        collect (list time pitch duration)))

(defun midimacs-score-notes-with-pauses (score)
  (let* ((notes (midimacs-score-sorted-notes score)))
    (loop for (time pitch duration) in notes
          with cum-time = (make-midimacs-time)
          append (let ((notes (midimacs-note-with-pause time pitch duration cum-time)))
                   (setq cum-time (midimacs-time+ time duration))
                   notes))))

(defun midimacs-note-with-pause (time pitch duration cum-time)
  (cond ((midimacs-time< time cum-time) (user-error "Score is polyphonic"))
        ((midimacs-time= time cum-time) (list (list time pitch duration)))
        ((midimacs-time> time cum-time) (list (list cum-time nil (midimacs-time- time cum-time))
                                              (list time pitch duration)))))

(defun midimacs-score-sorted-notes (score)
  (let ((notes (midimacs-score-notes score)))
    (cl-sort notes 'midimacs-time< :key (lambda (x) (elt x 0)))))

(defun midimacs-get-recording-score ()
  (let* ((code (midimacs-code-at-point))
         (event (midimacs-event-at-point))
         (track (midimacs-track-at-point))
         (score)
         (start-time (midimacs-event-start-time event)))

    (unless code
      (user-error "No code here"))

    (midimacs-code-open-window code)
    (setq score (make-midimacs-score :buffer (current-buffer)
                                     :channel (midimacs-track-channel track)
                                     :start-time start-time))

    (midimacs-code-insert-score code score)

    (other-window 1) ;; switch back to seq
    score))

(defun midimacs-code-insert-score (code score)
  (goto-char (point-min))
  (if (search-forward "(midimacs-score" nil t)
      (progn
        (forward-line)
        (midimacs-score-update-buffer score))
    (search-forward ";; run")
    (search-forward "(lambda ")
    (end-of-line)
    (insert "\n")
    (insert "\n")
    (insert (midimacs-score-text score)))

  (eval-buffer))

(defun midimacs-split-score (code-name)
  (interactive "cCode name: ")
  (let* ((track (midimacs-track-at-point))
         (beat (midimacs-beat-at-point))
         (event (midimacs-track-event-at-beat track beat)))
    
    (unless event
      (user-error "No event here"))

    (let* ((code (midimacs-event-code event))
           (score (midimacs-code-extract-score code))
           (offset (midimacs-time- (make-midimacs-time :beat beat)
                                   (midimacs-event-start-time event)))
           (new-code))
      
      (unless score
        (user-error "No score"))

      (setq new-code (midimacs-maybe-create-code code-name))

      (save-excursion
        (delete-char 1)
        (insert (string code-name)))

      (destructuring-bind (prev-score new-score)
          (midimacs-score-split score offset)
        (midimacs-code-replace-score code prev-score)

        (midimacs-code-open-window code)
        (erase-buffer)
        (insert (midimacs-code-text code))
        (eval-buffer)
        (other-window 1)

        (midimacs-code-open-window new-code)
        (setf (midimacs-score-buffer new-score) (current-buffer))
        (midimacs-code-insert-score new-code new-score)
        (midimacs-code-replace-score code prev-score)
        (eval-buffer)
        (other-window 1)))))

(defun midimacs-score-split (score offset)
  (let* ((notes (midimacs-score-notes score))
         (notes-a (loop for (time pitch duration) in notes
                        if (midimacs-time< time offset)
                        collect (list time pitch duration)))
         (notes-b (loop for (time pitch duration) in notes
                        if (midimacs-time>= time offset)
                        collect (list (midimacs-time- time offset) pitch duration)))
         (score-a (make-midimacs-score :notes notes-a))
         (score-b (make-midimacs-score :notes notes-b)))
    (list score-a score-b)))

(defun midimacs-merge-scores ()
  (interactive)
  (let* ((track (midimacs-track-at-point))
         (beat (midimacs-beat-at-point))
         (event (midimacs-track-event-at-beat track beat))
         (prev-event (midimacs-track-event-at-beat track (1- beat))))

    (unless (and event prev-event (midimacs-event-do-init event))
      (user-error "No event and prev-event"))

    (let* ((code (midimacs-event-code event))
           (prev-code (midimacs-event-code prev-event))
           (score (midimacs-code-extract-score code))
           (prev-score (midimacs-code-extract-score prev-code))
           (offset (midimacs-time- (midimacs-event-start-time event)
                                   (midimacs-event-start-time prev-event))))

      (unless (and score prev-score)
        (user-error "No score and prev-score"))

      (save-excursion
        (delete-char 1)
        (insert "."))

      (midimacs-code-replace-score prev-code (midimacs-merge-score prev-score score offset))
      (midimacs-code-open-window prev-code)
      (erase-buffer)
      (insert (midimacs-code-text prev-code))
      (eval-buffer)
      (other-window 1))))

(defun midimacs-code-replace-score (code score)
  (destructuring-bind (before notes after)
      (midimacs-score-split-text (midimacs-code-text code))
    (setf (midimacs-code-text code)
          (concat before
                  (midimacs-score-text score)
                  after))))

(defun midimacs-merge-score (score-a score-b offset)
  (let* ((notes-a (midimacs-score-notes score-a))
         (notes-b (midimacs-score-notes score-b))
         (notes (append notes-a
                        (loop for (time pitch duration) in notes-b
                              collect (list (midimacs-time+ offset time) pitch duration)))))
    (make-midimacs-score :notes notes)))

(defun midimacs-code-extract-score (code)
  (let* ((text (midimacs-code-text code))
         (score-text (elt (midimacs-score-split-text text) 1)))
    (midimacs-parse-score score-text)))

(defun midimacs-parse-score (score-text)
  (let* ((lines (split-string score-text "\n+"))
         (note-lines (cdr lines))
         (cum-time (make-midimacs-time))
         (notes (loop for line in note-lines
                      if (string-match "[^ ]" line)
                      collect (destructuring-bind (time pitch duration)
                                  (midimacs-parse-note-line line)
                                (unless time
                                  (setq time cum-time))
                                (setq cum-time (midimacs-time+ cum-time duration))
                                (list time pitch duration)))))
    (make-midimacs-score :notes notes)))

(defun midimacs-parse-note-line (line)
  (let* ((regex (concat "^"
                        " *"
                        "( *(?"
                        " *"
                        "\\([^ ]+\\)"
                        " +"
                        "\\([^ )]+\\)"
                        "\\(?:"
                        " +"
                        "\\([^ )]+\\)"
                        "\\)?"
                        " *"
                        ") *)? *)?"
                        " *$"))
         (m1) (m2) (m3)
         (time) (pitch) (duration))

    (string-match regex line)
    (setq m1 (match-string 1 line))
    (setq m2 (match-string 2 line))
    (setq m3 (match-string 3 line))

    (if m3
        (progn
          (setq time (midimacs-parse-time m1))
          (setq pitch (midimacs-parse-pitch m2))
          (setq duration (midimacs-parse-time m3)))
      (setq pitch (midimacs-parse-pitch m1))
      (setq duration (midimacs-parse-time m2)))
    (list time pitch duration)))

(defun midimacs-score-split-text (text)
  (let* ((anything-regex (concat
                          "\\("
                          "\\(?:.\\|\n\\)+"
                          "\n"
                          "\\)"))
         (score-regex (concat
                       "\\("
                       " *(midimacs-score *\n"
                       " *("
                       "\\(?:"
                       " *(.+) *\n*"
                       "\\)+"
                       " *) *)"
                       "\\)"))
         (regex (concat
                 "^"
                 anything-regex
                 score-regex
                 anything-regex
                 "$")))
    (string-match regex text)
    (list (match-string 1 text)
          (match-string 2 text)
          (match-string 3 text))))

(defun midimacs-code-score-hide-times ()
  (interactive)
  (let ((text (buffer-string))
        (p (point)))
    (destructuring-bind (before score-text after)
        (midimacs-score-split-text text)
      (erase-buffer)
      (insert before)
      (insert (midimacs-score-text (midimacs-parse-score score-text) :hide-times t))
      (insert after)
      (goto-char p))))

(defun midimacs-code-score-show-times ()
  (interactive)
  (let ((text (buffer-string))
        (p (point)))
    (destructuring-bind (before score-text after)
        (midimacs-score-split-text text)
      (erase-buffer)
      (insert before)
      (insert (midimacs-score-text (midimacs-parse-score score-text)))
      (insert after)
      (goto-char p))))

(defun midimacs-recording-score-clear-ahead ()
  (midimacs-remove-note-from-score
   midimacs-recording-score
   (midimacs-time- midimacs-song-time
                   (midimacs-score-start-time midimacs-recording-score)
                   (make-midimacs-time :tick -1)))) ;; one ahead

(provide 'midimacs-score)

;; Local variables:
;; byte-compile-warnings: (not cl-functions)
;; End:
