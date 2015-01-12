;; TODO:
;;
;; midimacs-quantize-region
;; midi event history per channel

;; BUGS:
;; cannot save when code doesn't parse
;; when midi server goes away we die

(eval-when-compile
  (require 'cl)
  (require 'heap)
  (require 'picture))

(define-derived-mode midimacs-seq-mode fundamental-mode "midimacs-seq-mode"
  (midimacs-extend-picture-mode)
  (setq buffer-read-only nil)
  (local-set-key (kbd "C-x C-[") 'midimacs-set-repeat-start)
  (local-set-key (kbd "C-x C-]") 'midimacs-set-repeat-end)
  (local-set-key (kbd "RET") 'midimacs-seq-enter)
  (local-set-key (kbd "C-x SPC") 'midimacs-toggle-play)
  (local-set-key (kbd "M-SPC") 'midimacs-toggle-play)
  (local-set-key (kbd "C-M-SPC") 'midimacs-play-here)
  (local-set-key (kbd "C-<return>") 'midimacs-position-here)
  (local-set-key (kbd "C-x C-r") 'midimacs-record-keyboard)
  (local-set-key (kbd "C-x C-s") 'midimacs-save)
  (local-set-key (kbd "C-x C-w") 'midimacs-save-as)
  (local-set-key (kbd "C-x C-f") 'midimacs-open)
  (local-set-key (kbd "C-x T") 'midimacs-tap-tempo-and-play)
  (local-set-key (kbd "C-x t") 'midimacs-tap-tempo)
  (setq-local after-change-functions '(midimacs-seq-after-change))
  (setq-local transient-mark-mode nil)
  (setq-local font-lock-defaults `(((,(midimacs-bad-track-regex) . font-lock-warning-face))))
  (setq truncate-lines t))

(define-derived-mode midimacs-seq-record-keyboard-mode midimacs-seq-mode "midimacs-seq-record-keyboard-mode"
  (setq buffer-read-only t)
  (let ((keyboard-notes (midimacs-keyboard-notes)))
    (loop for (char . note) in keyboard-notes
          do (local-set-key (kbd (string char)) (lexical-let ((pitch (midimacs-parse-pitch note)))
                                                  (lambda ()
                                                    (interactive)
                                                    (midimacs-record-key pitch)))))))

(define-derived-mode midimacs-code-mode emacs-lisp-mode "midimacs-code-mode"
  (define-key midimacs-code-mode-map (kbd "M-SPC") 'midimacs-toggle-play)
  (define-key midimacs-code-mode-map (kbd "C-x C-s") 'midimacs-save)
  (define-key midimacs-code-mode-map (kbd "C-x C-w") 'midimacs-save-as)
  (define-key midimacs-code-mode-map (kbd "C-x C-f") 'midimacs-open)
  (define-key midimacs-code-mode-map (kbd "C-c C-c") 'midimacs-code-update))

;;;###autoload
(defun midimacs () "Start midimacs"
  (interactive)
  (switch-to-buffer (midimacs-buffer-seq-name))
  (midimacs-seq-mode)
  (midimacs-init))

(defcustom midimacs-length 256
  "number of beats"
  :group 'midimacs)

(defcustom midimacs-bpm 120
  "beats per minute"
  :group 'midimacs)

(defconst midimacs-ticks-per-beat 24)
(defconst midimacs-pitch-numbers '(("c" . 0)
                                   ("d" . 2)
                                   ("e" . 4)
                                   ("f" . 5)
                                   ("g" . 7)
                                   ("a" . 9)
                                   ("b" . 11)))
(defconst midimacs-accidental-numbers '((nil . 0)
                                        ("b" . -1)
                                        ("s" . +1)))

(defconst midimacs-pitch-names '((0 . C)
                                 (1 . Cs)
                                 (2 . D)
                                 (3 . Ds)
                                 (4 . E)
                                 (5 . F)
                                 (6 . Fs)
                                 (7 . G)
                                 (8 . Gs)
                                 (9 . A)
                                 (10 . As)
                                 (11 . b)))

(defconst midimacs-left-bar-length 4)
(defconst midimacs-top-bar-height 1)
(defconst midimacs-track-char ?>)
(defconst midimacs-sustain-char ?.)
(defconst midimacs-space-char ? )
(defconst midimacs-default-recording-duration (midimacs-parse-time '1/2))

(defvar midimacs-tracks '())
(defvar midimacs-codes nil)
(defvar midimacs-repeat-start nil)
(defvar midimacs-repeat-end midimacs-length)
(defvar midimacs-repeat-end nil)
(defvar midimacs-abs-time nil)
(defvar midimacs-song-time nil)
(defvar midimacs-state 'stopped)
(defvar midimacs-start-time-seconds nil)
(defvar midimacs-last-tick-seconds nil) ; used for recording
(defvar midimacs-filename nil)
(defvar midimacs-amidicat-proc nil)
(defvar midimacs-scheduled-note-offs nil)
(defvar midimacs-repeat-start-overlay nil)
(defvar midimacs-repeat-end-overlay nil)
(defvar midimacs-play-overlay nil)
(defvar midimacs-midi-history nil)
(defvar midimacs-recording-score nil)

(defface midimacs-top-bar-background-face
  '((t (:foreground "dim gray")))
  "Face for top bar number markers"
  :group 'midimacs)

(defface midimacs-repeat-face
  '((t (:foreground "gold")))
  "Face for repeat markers"
  :group 'midimacs)

(defface midimacs-play-face
  '((t (:foreground "yellow")))
  "Face for play symbols"
  :group 'midimacs)

(defstruct midimacs-event
  code-name
  start-time
  do-init)

(defstruct midimacs-code
  name
  text
  init
  run)

(defstruct midimacs-track
  channel
  events
  (state nil)
  (last-init-time (make-midimacs-time)))

(defstruct midimacs-midi-message
  status
  (data1 nil)
  (data2 nil))

(defstruct midimacs-time
  (beat 0)
  (tick 0))

(defstruct midimacs-score
  notes
  buffer
  channel
  start-time)

(defun midimacs-init ()
  (setq midimacs-tracks '())
  (setq midimacs-codes (make-hash-table :test 'equal))
  (setq midimacs-song-time (make-midimacs-time))
  (setq midimacs-abs-time (make-midimacs-time))
  (setq midimacs-state 'stopped)
  (setq midimacs-start-time-seconds nil)
  (setq midimacs-filename nil)
  (setq midimacs-scheduled-note-offs (midimacs-make-scheduled-note-offs-heap))
  (setq midimacs-repeat-start (make-midimacs-time))
  (setq midimacs-repeat-end (make-midimacs-time :beat midimacs-length))
  (setq midimacs-repeat-start-overlay nil)
  (setq midimacs-repeat-end-overlay nil)
  (setq midimacs-play-overlay nil)
  (setq midimacs-recording-score nil)

  (midimacs-close-all-code-buffers)
  (midimacs-amidicat-proc-init)
  (midimacs-draw ""))

(defun midimacs-extend-picture-mode ()
  (use-local-map picture-mode-map)
  (set (make-local-variable 'picture-killed-rectangle) nil)
  (set (make-local-variable 'tab-stop-list) (default-value 'tab-stop-list))
  (set (make-local-variable 'picture-tab-chars)
       (default-value 'picture-tab-chars))
  (make-local-variable 'picture-vertical-step)
  (make-local-variable 'picture-horizontal-step)
  (set (make-local-variable 'picture-mode-old-truncate-lines) truncate-lines)
  (setq truncate-lines t)
  (picture-set-motion 0 1))

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

      (setq midimacs-tracks
            (remove nil (loop while (< (point) (point-max))
                              collect (let* ((track-s (buffer-substring (point) (line-end-position)))
                                             (track (midimacs-parse-track track-s)))
                                        (end-of-line)
                                        (when (< (point) (point-max)) (forward-char))
                                        track)))))))

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
  (let ((chars (string-to-list codes-s))
        (current nil)
        (start-time nil))
    (loop for c being the elements of chars
          using (index i)
          collect (cond ((= c midimacs-space-char)
                         (setq current nil)
                         nil)
                        ((= c midimacs-sustain-char)
                         (when current
                           (make-midimacs-event :code-name current
                                                :start-time start-time)))
                        (t
                         (setq current c)
                         (setq start-time (make-midimacs-time :beat i))
                         (make-midimacs-event :code-name c
                                              :start-time start-time
                                              :do-init t))))))

(defun midimacs-maybe-create-code (code-name)
  (unless (midimacs-get-code code-name)
    (let ((code (make-midimacs-code :name code-name
                                    :text (midimacs-code-template code-name))))
      (puthash code-name code midimacs-codes))))

(defun midimacs-seq-buffer-contents ()
  (with-current-buffer (midimacs-buffer-seq)
    (save-excursion
      (goto-char (point-min))
      (end-of-line)
      (forward-char)
      (buffer-substring (point) (point-max)))))

(defun midimacs-amidicat-proc-init ()
  (interactive)
  (let ((buffer-name (midimacs-amidicat-buffer-name))
        (process-name "midimacs-amidicat"))
    (when (get-buffer buffer-name)
      (when (get-process process-name)
        (progn
          (midimacs-midi-flush-note-offs)
          (delete-process process-name)))
      (kill-buffer buffer-name))

    (setq midimacs-amidicat-proc
          (start-process process-name buffer-name
                         "amidicat" "--hex" "--port" "129:0" "--noread"))
    (set-process-filter midimacs-amidicat-proc 'midimacs-amidicat-read)))

(defun midimacs-draw (&optional contents)
  (with-current-buffer (midimacs-buffer-seq)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (midimacs-draw-top-bar)
      (when contents
        (insert contents))
      (goto-char (point-min)))))

(defun midimacs-draw-top-bar ()
  (with-current-buffer (midimacs-buffer-seq)
    (let ((inhibit-read-only t)
          (p (point)))
      (goto-char (point-min))
      (midimacs-draw-top-bar-numbers)
      (midimacs-draw-top-bar-background-overlay)
      (midimacs-make-top-bar-read-only)
      (midimacs-redraw-repeat-start)
      (midimacs-redraw-repeat-end)
      (midimacs-redraw-play)
      (end-of-line)
      (insert "\n"))))

(defun midimacs-redraw-repeat-start ()
  (unless (and midimacs-repeat-start-overlay (overlay-buffer midimacs-repeat-start-overlay))
    (setq midimacs-repeat-start-overlay (make-overlay 0 1))
    (overlay-put midimacs-repeat-start-overlay 'display "[")
    (overlay-put midimacs-repeat-start-overlay 'face 'midimacs-repeat-face))
  (let ((pos (+ (midimacs-time-beat midimacs-repeat-start) midimacs-left-bar-length)))
    (move-overlay midimacs-repeat-start-overlay pos (1+ pos))))

(defun midimacs-redraw-repeat-end ()
  (unless (and midimacs-repeat-end-overlay (overlay-buffer midimacs-repeat-end-overlay))
    (setq midimacs-repeat-end-overlay (make-overlay 0 1))
    (overlay-put midimacs-repeat-end-overlay 'display "]")
    (overlay-put midimacs-repeat-end-overlay 'face 'midimacs-repeat-face))
  (let ((pos (+ 1 (midimacs-time-beat midimacs-repeat-end) midimacs-left-bar-length)))
    (move-overlay midimacs-repeat-end-overlay pos (1+ pos))))

(defun midimacs-redraw-play ()
  (unless (and midimacs-play-overlay (overlay-buffer midimacs-play-overlay))
    (setq midimacs-play-overlay (make-overlay 0 1))
    (overlay-put midimacs-play-overlay 'face 'midimacs-play-face))
  (overlay-put midimacs-play-overlay 'display (midimacs-play-symbol))
  (let ((pos (+ 1 (midimacs-time-beat midimacs-song-time) midimacs-left-bar-length)))
    (move-overlay midimacs-play-overlay pos (1+ pos))))

(defun midimacs-draw-top-bar-numbers ()
  (goto-char (point-min))
  (delete-region (point) (line-end-position))
  (insert (make-string midimacs-left-bar-length ? ))
  (loop for i from 0 below midimacs-length by 4
        do (insert (format "%-4d" i))))

(defun midimacs-draw-top-bar-background-overlay ()
  (goto-char (point-min))
  (let ((overlay (make-overlay (point) (line-end-position))))
    (overlay-put overlay 'face 'midimacs-top-bar-background-face)))

(defun midimacs-make-top-bar-read-only ()
  (goto-char (point-min))
  (put-text-property (point) (line-end-position) 'read-only t))

(defun midimacs-play-symbol ()
  (cond ((eq midimacs-state 'playing)   "▶")
        ((eq midimacs-state 'recording) "●")
        ((eq midimacs-state 'stopped)   "◾")))

(defun midimacs-track-at-point ()
  (with-current-buffer (midimacs-buffer-seq)
    (save-excursion
      (beginning-of-line)
      (midimacs-parse-track (buffer-substring (point) (line-end-position))))))

(defun midimacs-beat-at-point ()
  (with-current-buffer (midimacs-buffer-seq)
    (midimacs-beat-at-column (current-column))))

(defun midimacs-seq-enter ()
  (interactive)
  (let ((code (midimacs-code-at-point)))
    (when code
      (midimacs-code-open-window code))))

(defun midimacs-set-repeat-start ()
  (interactive)
  (save-excursion
    (forward-char)
    (let ((new-repeat-start (make-midimacs-time :beat (midimacs-beat-at-point))))
      (midimacs-check-acceptable-repeat new-repeat-start midimacs-repeat-end)
      (setq midimacs-repeat-start new-repeat-start)))
  (midimacs-redraw-repeat-start))

(defun midimacs-set-repeat-end ()
  (interactive)
  (let ((new-repeat-end (make-midimacs-time :beat (midimacs-beat-at-point))))
    (midimacs-check-acceptable-repeat midimacs-repeat-start new-repeat-end)
    (setq midimacs-repeat-end new-repeat-end))
  (midimacs-redraw-repeat-end))

(defun midimacs-check-acceptable-repeat (start end)
  (let ((beats (- (midimacs-time-beat start) (midimacs-time-beat end))))
    (when (= beats 0)
      (user-error "repeat start and repeat end must be different"))))

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

(defun midimacs-track-set-event-at-beat (track beat event)
  (aset (midimacs-track-events track) beat event))

(defun midimacs-track-remove-event-at-beat (track time)
  (aset (midimacs-track-events track) beat nil))

(defun midimacs-track-events-at-beat (beat)
  (let ((event))
    (loop for track in midimacs-tracks
          if (setq event (midimacs-track-event-at-beat track beat))
          collect (list track event))))

(defun midimacs-event-rel-time (event time)
  (let ((event-time (midimacs-event-start-time event)))
    (midimacs-time- time event-time)))

(defun midimacs-check-beat (beat)
  (unless (and beat (>= beat 0))
    (user-error "No beat here")))

(defun midimacs-get-code (code-name)
  (gethash code-name midimacs-codes))

(defun midimacs-code-open-window (code)
  (let* ((buffer-name (midimacs-buffer-code-name (midimacs-code-name code)))
         (buffer-existed (get-buffer buffer-name))
         (visible-window (get-buffer-window buffer-name))
         (buffer (get-buffer-create buffer-name)))

    (unless buffer-existed
      (with-current-buffer buffer
        (insert (midimacs-code-text code))
        (beginning-of-buffer)
        (midimacs-code-mode)))

    (if visible-window
        (select-window visible-window)
      (when (one-window-p t)
        (split-window))
      (other-window 1))

    (switch-to-buffer buffer)))

(defun midimacs-beat-at-column (col)
  (when (>= col midimacs-left-bar-length)
    (- col midimacs-left-bar-length)))

(defun midimacs-string-member (c s)
  (member (if (stringp c)
              (string-to-char c)
            c)
          (string-to-list s)))

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

(defun midimacs-event-code (track-event)
  (let ((code-name (midimacs-event-code-name track-event)))
    (midimacs-get-code code-name)))

(defun midimacs-code-template (code-name)
  (concat
   "(midimacs-code
 ?" (string code-name) "

 ;; init
 (lambda (channel song-time state)

   nil)

 ;; run
 (lambda (channel song-time rel-time state)

   state)

 )
"))

(defun midimacs-code-update ()
  (interactive)
  (eval-buffer))

(defun midimacs-code (name init run)
  (let ((code (midimacs-get-code name)))
    (setf (midimacs-code-init code) init
          (midimacs-code-run code) run
          (midimacs-code-text code) (buffer-string)))
  (message (concat "updated code " (string name))))

(cl-defun midimacs-play-note (channel pitch-raw duration-raw &optional (velocity 100) (off-velocity 0))
  (setq pitch (cond ((symbolp pitch-raw) (midimacs-parse-pitch (symbol-name pitch-raw)))
                    ((stringp pitch-raw) (midimacs-parse-pitch pitch-raw))
                    (t pitch-raw)))

  (setq duration (cond ((symbolp duration-raw) (midimacs-parse-time (symbol-name duration-raw)))
                       ((stringp duration-raw) (midimacs-parse-time duration-raw))
                       ((numberp duration-raw)
                        (make-midimacs-time :beat (floor (/ duration-raw midimacs-ticks-per-beat))
                                            :tick (mod duration-raw midimacs-ticks-per-beat)))
                       (t duration-raw)))

  (midimacs-midi-schedule-note-off (midimacs-time+ midimacs-abs-time duration)
                                   (midimacs-midi-message-note-off channel pitch off-velocity))
  (midimacs-midi-execute (midimacs-midi-message-note-on channel pitch velocity)))

(cl-defun midimacs-play-notes (channel pitches-raw duration-raw &optional (velocity 100) (off-velocity 0))
  (loop for pitch-raw in pitches-raw
        do (midimacs-play-note channel pitch-raw duration-raw velocity off-velocity)))

(defun midimacs-midi-message-note-on (channel pitch velocity)
  (make-midimacs-midi-message :status (+ #x90 channel)
                              :data1 pitch
                              :data2 velocity))

(defun midimacs-midi-message-note-off (channel pitch velocity)
  (make-midimacs-midi-message :status (+ #x80 channel)
                              :data1 pitch
                              :data2 velocity))

(defun midimacs-midi-message-program-change (channel program)
  (make-midimacs-midi-message :status (+ #xC0 channel)
                              :data1 program))

(defun midimacs-program-change (channel program)
  (midimacs-midi-execute (midimacs-midi-message-program-change channel program)))

(defun midimacs-midi-schedule-note-off (abs-time message)
  (heap-add midimacs-scheduled-note-offs (list abs-time message)))

(defun midimacs-midi-execute (message)
  (process-send-string midimacs-amidicat-proc (midimacs-midi-serialize message)))

(defun midimacs-midi-serialize (message)
  (let* ((status (midimacs-midi-message-status message))
         (data1 (midimacs-midi-message-data1 message))
         (data2 (midimacs-midi-message-data2 message))
         (status-string (format "%02X" status))
         (data1-string (if data1 (format " %02X" data1) ""))
         (data2-string (if (and data1 data2) (format " %02X" data2) "")))
    (concat status-string data1-string data2-string "\n")))

(defun midimacs-midi-flush-note-offs ()
  (let ((el))
    (loop while (setq el (heap-delete-root midimacs-scheduled-note-offs))
          do (midimacs-midi-execute (nth 1 el)))))

(defun midimacs-amidicat-buffer-name ()
  "*midimacs-amidicat*")

(defun midimacs-amidicat-read (proc string)
                                        ; TODO
  (message string))

(defun midimacs-toggle-play ()
  (interactive)
  (cond ((eq midimacs-state 'playing) (midimacs-stop))
        ((eq midimacs-state 'recording) (midimacs-stop-recording))
        ((eq midimacs-state 'stopped) (midimacs-play))))

(defun midimacs-position-here ()
  (interactive)
  (let ((beat (midimacs-beat-at-point)))
    (midimacs-check-beat beat)
    (setq midimacs-song-time (make-midimacs-time :beat beat)))
  (midimacs-redraw-play))

(defun midimacs-play-here ()
  (interactive)
  (midimacs-position-here)
  (when (eq midimacs-state 'stopped)
    (midimacs-play)))

(defun midimacs-play ()
  (setq midimacs-state 'playing)
  (setq midimacs-start-time-seconds (float-time))
  (setq midimacs-abs-time (make-midimacs-time))
  (midimacs-redraw-play)
  (midimacs-tick))

(defun midimacs-record-keyboard ()
  (interactive)
  (if (eq midimacs-state 'stopped)
      (progn
        (midimacs-seq-record-keyboard-mode)
        (midimacs-record))
    (user-error "Can only start recording from stopped")))

(defun midimacs-record ()
  (setq midimacs-recording-score (midimacs-get-recording-score)) ;; can fail and die here
  (setq midimacs-state 'recording)
  (setq midimacs-start-time-seconds (float-time))
  (setq midimacs-abs-time (make-midimacs-time))
  (midimacs-redraw-play)
  (midimacs-tick))

(defun midimacs-tick ()
  (when (memq midimacs-state '(playing recording))

    (midimacs-trigger-note-offs)
    (midimacs-trigger-events)
    (midimacs-incr-position)
    (setq midimacs-last-tick-seconds (float-time))

    (when (eq midimacs-state 'recording)
      (midimacs-remove-note-from-score
       midimacs-recording-score
       (midimacs-time- midimacs-song-time
                       (midimacs-score-start-time midimacs-recording-score)
                       (make-midimacs-time :tick -1)))) ;; one ahead

    ;; only update when we have to
    (when (= (midimacs-time-tick midimacs-song-time) 0)
      (midimacs-redraw-play))

    (run-at-time (midimacs-wait-time) nil 'midimacs-tick)))

(defun midimacs-incr-position ()
  (let ((1-tick (make-midimacs-time :tick 1)))
    (setq midimacs-abs-time (midimacs-time+ midimacs-abs-time 1-tick))
    (setq midimacs-song-time (midimacs-time+ midimacs-song-time 1-tick))

    (when (midimacs-time= midimacs-song-time midimacs-repeat-end)
      (setq midimacs-song-time midimacs-repeat-start))))

(defun midimacs-trigger-note-offs ()
  (let ((note-offs (midimacs-delete-message-heap-until midimacs-scheduled-note-offs midimacs-abs-time)))
    (dolist (note-off note-offs)
      (midimacs-midi-execute note-off))))

(defun midimacs-delete-message-heap-until (heap pos)
  (let ((el))
    (loop while (and (setq el (heap-root heap))
                     (midimacs-time<= (nth 0 el) pos))
          collect (nth 1 (heap-delete-root heap)))))

(defun midimacs-trigger-events ()
  (loop for (track event) in (midimacs-track-events-at-beat (midimacs-time-beat midimacs-song-time))
        do (let* ((code (midimacs-event-code event))
                  (init (midimacs-code-init code))
                  (run (midimacs-code-run code))
                  (channel (midimacs-track-channel track)))

             (when (and init
                        (midimacs-event-start-do-init event)
                        (eq (midimacs-time-tick midimacs-song-time) 0))

               (setf (midimacs-track-state track)
                     (funcall init
                              channel
                              midimacs-song-time
                              (midimacs-track-state track)))

               (setf (midimacs-track-last-init-time track) midimacs-abs-time))

             (when run
               (setf (midimacs-track-state track)
                     (funcall run
                              channel
                              midimacs-song-time
                              (midimacs-event-rel-time event midimacs-song-time)
                              (midimacs-track-state track)))))))

(defun midimacs-wait-time ()
  (let* ((tick-sec (/ 60.0 midimacs-bpm midimacs-ticks-per-beat))
         (expected-time-sec (+ midimacs-start-time-seconds
                               (* tick-sec (midimacs-time-to-ticks midimacs-abs-time))))
         (now (float-time))
         (drift (- now expected-time-sec)))
    (max 0 (- tick-sec drift))))

(defun midimacs-stop ()
  (midimacs-midi-flush-note-offs)
  (setq midimacs-state 'stopped)
  (midimacs-redraw-play))

(defun midimacs-stop-recording ()
  (midimacs-stop)

  (with-current-buffer (midimacs-buffer-seq)
    (midimacs-seq-mode))
  (with-current-buffer (midimacs-score-buffer midimacs-recording-score)
    (setq midimacs-recording-score nil)
    (midimacs-code-update)))

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

(defun midimacs-time-to-ticks (time)
  (+ (midimacs-time-tick time)
     (* (midimacs-time-beat time) midimacs-ticks-per-beat)))

(defun midimacs-ticks-to-time (ticks)
  (let ((beat (floor (/ ticks midimacs-ticks-per-beat)))
        (tick (% ticks midimacs-ticks-per-beat)))
    (make-midimacs-time :beat beat
                        :tick tick)))

(defun midimacs-time< (a b)
  (let ((beat-a (midimacs-time-beat a))
        (beat-b (midimacs-time-beat b))
        (tick-a (midimacs-time-tick a))
        (tick-b (midimacs-time-tick b)))

    (cond ((< beat-a beat-b) t)
          ((and (= beat-a beat-b) (< tick-a tick-b)) t))))

(defun midimacs-time= (a b)
  (and (= (midimacs-time-beat a) (midimacs-time-beat b))
       (= (midimacs-time-tick a) (midimacs-time-tick b))))

(defun midimacs-time<= (a b)
  (or (midimacs-time= a b) (midimacs-time< a b)))

(defun midimacs-time> (a b)
  (not (midimacs-time<= a b)))

(defun midimacs-time>= (a b)
  (not (midimacs-time< a b)))

(defun midimacs-time+ (&rest times)
  (let ((ticks (loop for time in times
                     sum (midimacs-time-to-ticks time))))
    (midimacs-ticks-to-time ticks)))

(defun midimacs-time- (&rest times)
  (let ((ticks (cond ((= (length times) 0) 0)
                     ((= (length times) 1) (- (midimacs-time-to-ticks (car times))))
                     (t (let* ((car-ticks (midimacs-time-to-ticks (car times)))
                               (ticks (loop for time in (cdr times)
                                            sum (midimacs-time-to-ticks time))))
                          (- car-ticks ticks))))))

    (midimacs-ticks-to-time ticks)))

(defun midimacs-make-scheduled-note-offs-heap ()
  (make-heap (lambda (a b) (midimacs-time< (nth 0 a) (nth 0 b)))))

(defun midimacs-time-regex ()
  (let ((beat-regex "\\([0-9]+\\)")
        (frac-regex "\\([0-9]+\\)/\\([0-9]+\\)"))
    (concat "^\\(?:"
            beat-regex
            "\\|"
            beat-regex "\\+" frac-regex
            "\\|"
            frac-regex
            "\\)$")))

(defun midimacs-parse-time (ss)
  (let ((s (midimacs-sym-or-num-to-string ss)))
    (if (string-match (midimacs-time-regex) s)
        (let* ((beat-s (or (match-string 1 s) (match-string 2 s)))
               (frac-num-s (or (match-string 3 s) (match-string 5 s)))
               (frac-denom-s (or (match-string 4 s) (match-string 6 s)))
               (beat (if beat-s (string-to-number beat-s) 0))
               (frac-num (when frac-num-s (string-to-number frac-num-s)))
               (frac-denom (when frac-denom-s (string-to-number frac-denom-s)))
               (tick (if frac-num (midimacs-frac-to-tick frac-num frac-denom) 0)))
          (make-midimacs-time :beat beat :tick tick))
      (error (format "Couldn't parse time \"%s\"" s)))))

(defun midimacs-frac-to-tick (num denom)
  (let ((tick (/ (float (* num midimacs-ticks-per-beat)) denom)))
    (unless (= (truncate tick) tick)
      (error (format "Invalid tick fraction %d/%d at %d ticks per beat (tick %f)"
                     num denom midimacs-ticks-per-beat tick)))
    (truncate tick)))

(defun midimacs-parse-pitch (ss)
  (let ((s (cond ((symbolp ss) (symbol-name ss))
                 (t ss))))
    (if (string-match "^\\([a-gA-G]\\)\\([sb]\\)?\\(-?[0-9]\\)$" s)
        (let* ((base (downcase (match-string 1 s)))
               (accidental (match-string 2 s))
               (octave (string-to-number (match-string 3 s)))
               (pitch (+ (* 12 (+ octave 1))
                         (+ (cdr (assoc base midimacs-pitch-numbers))
                            (cdr (assoc accidental midimacs-accidental-numbers))))))
          (unless (and (>= pitch 0)
                       (<= pitch 127))
            (error (format "Pitch \"%s\" (%d) is out of range 0 <= x <= 127" s pitch)))
          pitch)
      (error (format "Couldn't parse pitch \"%s\"" s)))))

(defun midimacs-sym-or-num-to-string (x)
  (cond ((stringp x) x)
        ((symbolp x) (symbol-name x))
        ((numberp x) (number-to-string x))))

(defun midimacs-time-to-string (time)
  (let* ((beat (midimacs-time-beat time))
         (tick (midimacs-time-tick time))
         (gcd (midimacs-gcd tick midimacs-ticks-per-beat))
         (frac-num (/ tick gcd))
         (frac-denom (/ midimacs-ticks-per-beat gcd)))
    (format "%s%s%s"
            (if (and (= beat 0) (not (= frac-num 0)))
                ""
              (format "%d" beat))
            (if (or (= beat 0) (= frac-num 0))
                ""
              "+")
            (if (= frac-num 0)
                ""
              (format "%d/%d" frac-num frac-denom)))))

(defun midimacs-gcd (a b)
    (cond
     ((< a b) (gcd a (- b a)))
     ((> a b) (gcd (- a b) b))
     (t a)))

(defun midimacs-pitch-to-string (pitch)
  (let ((octave (- (floor (/ pitch 12)) 1))
        (name (cdr (assoc (% pitch 12) midimacs-pitch-names))))
    (format "%s%d" name octave)))

(defmacro midimacs-score (notes &rest channel)
  "channel defaults to track channel"
  (let ((cum-time (make-midimacs-time)))
    (cons
     'progn
     (loop for symbols in notes
           for onset-sym = (when (= (length symbols) 3) (nth 0 symbols))
           for pitch-sym = (if (= (length symbols) 3) (nth 1 symbols) (nth 0 symbols))
           for dur-sym = (if (= (length symbols) 3) (nth 2 symbols) (nth 1 symbols))
           for onset = (if onset-sym (midimacs-parse-time onset-sym) cum-time)
           for pitch = (unless (eq '- pitch-sym) (midimacs-parse-pitch (symbol-name pitch-sym)))
           for dur = (midimacs-parse-time dur-sym)
           do (setq cum-time (midimacs-time+ cum-time dur))
           when pitch
           collect `(when (midimacs-time= rel-time ,onset)
                      (midimacs-play-note (or ,channel channel) ,pitch ,dur))))))

(defmacro midimacs-timed (timed-funcs)
  (cons
   'progn
   (loop for (onset-sym on-func dur-sym off-func) in timed-funcs
         for on-time = (midimacs-parse-time onset-sym)
         for dur = (when dur-sym (midimacs-parse-time dur-sym))
         for off-time = (when dur-sym (midimacs-time+ on-time dur))
         collect `(cond ((midimacs-time= rel-time ,on-time)
                         (setq state ((lambda (state) ,on-func) state)))
                        ((and ,off-time ,off-func (midimacs-time= rel-time ,off-time))
                         (setq state ((lambda (state) ,off-func) state)))))))

(defun midimacs-set-tempo (bpm)
  (interactive "nBeats per minute: ")
  (let ((state midimacs-state))
    (midimacs-stop)
    (setq midimacs-bpm bpm)
    (when (eq state 'playing) ; TODO: recording
      (midimacs-play))))

(defun midimacs-tap-tempo ()
  (interactive)
  (let ((tempo (midimacs-read-tempo-taps)))
    (setq midimacs-bpm tempo)
    (message (format "Setting tempo to %.2f" tempo))))

(defun midimacs-tap-tempo-and-play ()
  (interactive)
  (midimacs-tap-tempo)
  (when (eq midimacs-state 'stopped)
    (midimacs-play)))

(defun midimacs-read-tempo-taps ()
  (loop for prev-time = (float-time)
        for char = (read-char (if tempo
                                  (format "Current tempo: %.2f" tempo)
                                "Tap any key other than SPACE to start tapping, or SPACE to exit"))
        for tempo = (midimacs-calc-tap-tempo times)
        until (= char ? )
        collect (- (float-time) prev-time) into times
        finally (return tempo)))

(defun midimacs-calc-tap-tempo (times)
  (let ((good-times (cdr times)))
    (when good-times
      (/ 60 (/ (apply '+ good-times) (length good-times))))))

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

(defun midimacs-keyboard-notes ()
  '((?z . C2)
    (?s . Cs2)
    (?x . D2)
    (?d . Ds2)
    (?c . E2)
    (?v . F2)
    (?g . Fs2)
    (?b . G2)
    (?h . Gs2)
    (?n . A2)
    (?j . As2)
    (?m . B2)
    (?, . C3)
    (?l . Cs3)
    (?. . D3)
    (?\; . Ds3)
    (?/ . E3)
    (?q . C3)
    (?2 . Cs3)
    (?w . D3)
    (?3 . Ds3)
    (?e . E3)
    (?r . F3)
    (?5 . Fs3)
    (?t . G3)
    (?6 . Gs3)
    (?y . A3)
    (?7 . As3)
    (?u . B3)
    (?i . C4)
    (?9 . Cs4)
    (?o . D4)
    (?0 . Ds4)
    (?p . E4)
    (?\[ . F4)
    (?+ . Fs4)
    (?\] . G4)))

(defun midimacs-record-key (pitch)
  (let* ((song-time (midimacs-quantized-song-time))
         (duration midimacs-default-recording-duration)
         (channel (midimacs-score-channel midimacs-recording-score))
         (start-time (midimacs-score-start-time midimacs-recording-score))
         (rel-time (midimacs-time- song-time start-time)))

    (when (midimacs-time> rel-time (make-midimacs-time))
      (midimacs-play-note channel pitch duration)
      (midimacs-add-note-to-score midimacs-recording-score rel-time pitch duration))))

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

(defun midimacs-score-text (score)
  (let ((notes (midimacs-score-notes score)))
    (concat "   (midimacs-score\n"
            "    ("
            (apply 'concat (loop for (tm p d) being the elements of notes using (index i)
                                 collect (concat (if (= i 0) "(" "\n     (")
                                                 (format "%-9s %-3s %s"
                                                         (midimacs-time-to-string tm)
                                                         (midimacs-pitch-to-string p)
                                                         (midimacs-time-to-string d))
                                                 ")")))
            "))")))

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

    (beginning-of-buffer)
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

    (midimacs-code-update)

    (other-window 1) ;; switch back to seq
    score))

(defun midimacs-merge-scores ()
  (interactive)
  (let* ((track (midimacs-track-at-point))
         (beat (midimacs-beat-at-point))
         (event (midimacs-track-event-at-beat track beat))
         (prev-event (midimacs-track-event-at-beat track (1- beat))))

    (unless (and event prev-event)
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
  (let ((anything-regex (concat
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


(provide 'midimacs)
;;; midimacs.el ends here
