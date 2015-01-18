(eval-when-compile
  (require 'cl))
(require 'midimacs-globals)

(defun midimacs-seq-after-change (begin end length-before)
  (when (and (> (count-lines 1 end) 1)
             (not (and (= length-before 1)
                       (= (- end begin) 0))))
    (midimacs-set-tracks-from-buffer)))

(defun midimacs-set-tracks-from-buffer ()
  (with-current-buffer (midimacs-buffer-seq)
    (save-excursion
      (goto-char (point-min))
      (end-of-line)
      (forward-char)

      (let ((new-tracks (loop while (< (point) (point-max))
                              collect (let* ((track-s (buffer-substring (point) (line-end-position)))
                                             (track (midimacs-parse-track track-s)))
                                        (end-of-line)
                                        (when (< (point) (point-max)) (forward-char))
                                        track))))
        (setq midimacs-tracks
              (midimacs-tracks-add-state midimacs-tracks
                                         (remove nil new-tracks)))))))

(defun midimacs-tracks-add-state (old-tracks new-tracks)
  (if (midimacs-tracks-match midimacs-tracks new-tracks)
      (loop for old-track in old-tracks
            for new-track in new-tracks
            collect (progn
                      (setf (midimacs-track-state new-track)
                            (midimacs-track-state old-track))
                      new-track))
    (when (memq midimacs-state '(playing recording))
      (warn "Destructive edit, discarding state and stopping")
      (midimacs-stop))
    new-tracks))

(defun midimacs-tracks-match (tracks-a tracks-b)
  (and (= (length tracks-a) (length tracks-b))
       (loop for a in tracks-a
             for b in tracks-b
             always (= (midimacs-track-channel a)
                       (midimacs-track-channel b)))))

(defun midimacs-parse-track (line)
  (when (midimacs-string-is-track line)

    (let* ((channel-s (substring line 1 3))
           (channel (string-to-number channel-s))
           (events-s (substring line 4))
           (events (midimacs-parse-events events-s)))

      (loop for event in events
            if event
            do (midimacs-maybe-create-code (midimacs-event-code-name event)))

      (make-midimacs-track :channel channel
                           :events events))))

(defun midimacs-string-is-track (track-s)
  (let* ((channel-regex ">\\(0[0-9]\\|1[0-5]\\)")
         (track-regex
          (concat
           "^"
           channel-regex
           "..+"
           "$")))

    (string-match track-regex track-s)))

(defun midimacs-parse-events (codes-s)
  (let* ((chars (string-to-list codes-s))
         (events-raw (loop for c being the elements of chars
                           using (index i)
                           with current = nil
                           with start-time = nil
                           when (and (= c midimacs-sustain-char) current)
                                collect (make-midimacs-event :code-name current
                                                             :start-time start-time)
                           else when (and (not (= c midimacs-space-char))
                                          (not (= c midimacs-sustain-char)))
                                     collect (progn
                                               (setq current c)
                                               (setq start-time (make-midimacs-time :beat i))
                                               (make-midimacs-event :code-name current
                                                                    :start-time start-time
                                                                    :do-init t))
                                else
                                     collect (progn
                                               (setq current nil)
                                               nil))))

    ;; add end-time
    (reverse (loop for e being the elements of (reverse events-raw)
                   using (index ri)
                   with end-time = nil
                   for i = (- (length events-raw) ri 1)
                   when (not e)
                        collect (progn
                                  (setq end-time nil)
                                  nil)
                        else when (not end-time)
                             collect (progn
                                       (setq end-time (make-midimacs-time :beat i))
                                       (setf (midimacs-event-end-time e) end-time)
                                       e)
                             else
                             collect (progn
                                       (setf (midimacs-event-end-time e) end-time)
                                       e)))))

(defun midimacs-bad-track-regex ()
  (let* ((not-a-two-char-number
          (concat
           "\\("
           "."
           "[^0-9]"
           "\\|"
           "[^0-9]"
           "."
           "\\)"))

         (greater-than-15
          (concat
           "\\("
           "[2-9]."
           "\\|"
           "1[6-9]"
           "\\)"))

         (bad-channel
          (concat
           "\\("
           not-a-two-char-number
           "\\|"
           greater-than-15
           "\\)"))

         (less-than-10 "0[0-9]")
         (10-to-15 "1[0-5]")

         (good-channel
          (concat
           "\\("
           less-than-10
           "\\|"
           10-to-15
           "\\)"))

         (space " ")
         (not-space "[^ ]")

         (period-at-beginning "\\..*")
         (period-after-whitespace ".* \\..*")

         (bad-events
          (concat
           "\\("
           period-at-beginning
           "\\|"
           period-after-whitespace
           "\\)")))

    (concat
     "^"
     ">"
     "\\("
     bad-channel
     ".*"
     "\\|"
     good-channel
     not-space
     ".*"
     "\\|"
     good-channel
     space
     bad-events
     "\\)"
     "$")))


(provide 'midimacs-parse-seq)
