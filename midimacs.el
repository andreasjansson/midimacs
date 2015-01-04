;; TODO:
;; make all globals buffer-local to the seq buffer, include
;; some name in the buffer names.
;; backspace


;; BUGS:
;; cannot save when code doesn't parse
;; when midi server goes away we die

(eval-when-compile
  (require 'cl)
  (require 'heap))

(define-derived-mode midimacs-seq-mode special-mode "midimacs-seq-mode"
  (midimacs-seq-define-letter-keys)
  (define-key midimacs-seq-mode-map (kbd "[") 'midimacs-set-repeat-start)
  (define-key midimacs-seq-mode-map (kbd "]") 'midimacs-set-repeat-end)
  (define-key midimacs-seq-mode-map (kbd "C-c a") 'midimacs-add-track)
  (define-key midimacs-seq-mode-map (kbd "C-d") 'midimacs-remove-code)
  (define-key midimacs-seq-mode-map (kbd "DEL") 'midimacs-remove-code-backward)
  (define-key midimacs-seq-mode-map (kbd "RET") 'midimacs-seq-enter)
  (define-key midimacs-seq-mode-map (kbd "C-w") 'midimacs-seq-kill)
  (define-key midimacs-seq-mode-map (kbd "C-y") 'midimacs-seq-yank)
  (define-key midimacs-seq-mode-map (kbd "C-x SPC") 'midimacs-toggle-play)
  (define-key midimacs-seq-mode-map (kbd "SPC") 'midimacs-toggle-play)
  (define-key midimacs-seq-mode-map (kbd "M-SPC") 'midimacs-play-here)
  (define-key midimacs-seq-mode-map (kbd "C-<return>") 'midimacs-position-here)
  (define-key midimacs-seq-mode-map (kbd "C-x C-s") 'midimacs-save)
  (define-key midimacs-seq-mode-map (kbd "C-x C-w") 'midimacs-save-as)
  (define-key midimacs-seq-mode-map (kbd "C-x C-f") 'midimacs-open)
  (define-key midimacs-seq-mode-map (kbd "C-c SPC") 'midimacs-x-jump)
  (setq transient-mark-mode nil)
  (setq truncate-lines t))

(define-derived-mode midimacs-code-mode emacs-lisp-mode "midimacs-code-mode"
  (define-key midimacs-code-mode-map (kbd "C-x SPC") 'midimacs-toggle-play)
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

(defcustom midimacs-ticks-per-beat 24
  "number of ticks per beat"
  :group 'midimacs)

(defconst midimacs-letters "abcdefghijklmnopqrstuvwxyz")
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

(defvar midimacs-tracks '())
(defvar midimacs-codes nil)
(defvar midimacs-repeat-start nil)
(defvar midimacs-repeat-end midimacs-length)
(defvar midimacs-repeat-end nil)
(defvar midimacs-abs-time nil)
(defvar midimacs-song-time nil)
(defvar midimacs-state 'stopped)
(defvar midimacs-start-time-seconds nil)
(defvar midimacs-filename nil)
(defvar midimacs-amidicat-proc nil)
(defvar midimacs-scheduled-note-offs nil)
(defvar midimacs-selection nil)

;(defface midimacs-seq-region-face '((t :inherit 'region)) "Face for midimacs sequencer regions" :group 'midimacs-faces)

(defstruct midimacs-event
  code-name
  do-init)

(defstruct midimacs-code
  name
  text
  init
  run)

(defstruct midimacs-track
  name
  events
  state
  (last-init-time (make-midimacs-time)))

(defstruct midimacs-midi-message
  status
  (data1 nil)
  (data2 nil))

(defstruct midimacs-time
  (beat 0)
  (tick 0))

(defun midimacs-seq-define-letter-keys ()
  (dolist (c (string-to-list midimacs-letters))
    (lexical-let ((s (string c)))

      (define-key midimacs-seq-mode-map (kbd s)
        (lambda ()
          (interactive)
          (midimacs-add-code-init s nil)))

      (define-key midimacs-seq-mode-map (kbd (upcase s))
        (lambda ()
          (interactive)
          (midimacs-add-code-init s t))))))

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
  (setq midimacs-selection nil)

  (midimacs-code-close-all-buffers)
  (midimacs-amidicat-proc-init)
  (midimacs-draw))

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

(defun midimacs-draw ()
  (interactive)
  (with-current-buffer (midimacs-buffer-seq)
    (let ((inhibit-read-only t)
          (p (point)))
      (erase-buffer)
      (midimacs-draw-top-bar)
      (midimacs-draw-tracks)
      (goto-char p))))

(defun midimacs-draw-top-bar ()
  (let* ((p 0)
         (space (string-to-char " "))
         (repeat-start-col (+ (midimacs-time-beat midimacs-repeat-start) 1))
         (repeat-end-col (+ (midimacs-time-beat midimacs-repeat-end) 2))
         (position-col (+ (midimacs-time-beat midimacs-song-time) 2))
         (chars `((,repeat-start-col . "[")
                  (,repeat-end-col . "]")
                  (,position-col . ,(midimacs-play-symbol))))
         (sorted-chars (sort chars (lambda (a b)
                                     (< (car a) (car b))))))

    (loop for (col . s) in sorted-chars do
          (let ((spaces (- col p)))
            (insert (make-string spaces space))
            (insert s)
            (setq p (+ p spaces (length s)))))

    (insert (make-string (max 0 (+ (- midimacs-length p) 2)) space)))

  (insert "\n"))

(defun midimacs-play-symbol ()
  (cond ((eq midimacs-state 'playing) "▶")
        ((eq midimacs-state 'stopped) "◾")))

(defun midimacs-draw-tracks ()
  (dolist (track midimacs-tracks)
    (midimacs-draw-track track)))

(defun midimacs-draw-track (track)
  (insert (midimacs-track-name track))
  (insert " ")
  (midimacs-draw-track-events track)
  (insert "\n"))

(defun midimacs-draw-track-events (track)
  (let ((track-events (midimacs-track-events track)))
    (dotimes (i midimacs-length)
      (let* ((track-event (elt track-events i)))
        (midimacs-draw-track-event track-event)))))

(defun midimacs-draw-track-event (track-event)
  (insert (if (eq track-event nil)
              "-"
            (midimacs-event-string track-event))))

(defun midimacs-seq-enter ()
  (interactive)
  (let* ((track (midimacs-current-track))
         (beat (midimacs-current-beat))
         (time (make-midimacs-time :beat beat))
         (code (midimacs-track-code-at-beat track (midimacs-time-beat time))))
    (when (and track beat code)
      (midimacs-code-open-window code))))

(defun midimacs-seq-kill ()
  (interactive)
  (destructuring-bind (col-start line-start col-end line-end)
      (midimacs-selected-rect)
    (unless (and (midimacs-beat-at-column col-start)
                 (midimacs-track-at-line line-start)
                 (midimacs-beat-at-column col-end)
                 (midimacs-track-at-line line-end))
      (user-error "Cannot kill region without track or beat"))

    (setq midimacs-selection
          (loop for line from line-start to line-end
                collect (let ((track (midimacs-track-at-line line)))
                          (loop for col from col-start to col-end
                                collect (let* ((beat (midimacs-beat-at-column col))
                                               (event (midimacs-track-event-at-beat track beat)))
                                          (midimacs-track-remove-event-at-beat track beat)
                                          event))))))
  (goto-char (mark))
  (midimacs-draw))

(defun midimacs-column (pos)
  (save-excursion
    (goto-char pos)
    (current-column)))

(defun midimacs-selected-rect ()
  (let ((col-mark (midimacs-column (mark)))
        (line-mark (count-lines 1 (mark)))
        (col-point (midimacs-column (point)))
        (line-point (count-lines 1 (point))))
    (list (min col-mark col-point)
          (min line-mark line-point)
          (max col-mark col-point)
          (max line-mark line-point))))

(defun midimacs-set-repeat-start ()
  (interactive)
  (save-excursion
    (forward-char)
    (let ((new-repeat-start (make-midimacs-time :beat (midimacs-current-beat))))
      (midimacs-check-acceptable-repeat new-repeat-start midimacs-repeat-end)
      (setq midimacs-repeat-start new-repeat-start)))
  (midimacs-draw))

(defun midimacs-seq-yank ()
  (interactive)
  (when midimacs-selection
    (let* ((col-start (midimacs-column (point)))
           (line-start (count-lines 1 (point)))
           (cols (length (car midimacs-selection)))
           (lines (length midimacs-selection)))

      (loop for line below lines
            do (let ((track (midimacs-track-at-line (+ line-start line))))
                 (when track
                   (loop for col below cols
                         do (let ((beat (midimacs-beat-at-column (+ col-start col))))
                              (when beat
                                (midimacs-track-set-event-at-beat
                                 track beat (nth col (nth line midimacs-selection)))))))))))
  (midimacs-draw))

(defun midimacs-set-repeat-end ()
  (interactive)
  (let ((new-repeat-end (make-midimacs-time :beat (midimacs-current-beat))))
    (midimacs-check-acceptable-repeat midimacs-repeat-start new-repeat-end)
    (setq midimacs-repeat-end new-repeat-end))
  (midimacs-draw))

(defun midimacs-check-acceptable-repeat (start end)
  (let ((beats (- (midimacs-time-beat start) (midimacs-time-beat end))))
    (when (= beats 0)
      (user-error "repeat start and repeat end must be different"))))

(defun midimacs-add-track ()
  (interactive)
  (let ((track (make-midimacs-track :name (midimacs-next-track-name)
                               :events (make-vector midimacs-length nil)
                               :state nil)))
    (setq midimacs-tracks (append midimacs-tracks (list track))))

  (midimacs-draw))

(defun midimacs-next-track-name ()
  (string (elt midimacs-letters (length midimacs-tracks))))

(defun midimacs-track-event-at-beat (track beat)
  (elt (midimacs-track-events track) beat))

(defun midimacs-track-code-at-beat (track beat)
  (let ((event (midimacs-track-event-at-beat track beat)))
    (when event
      (midimacs-event-code event))))

(defun midimacs-track-rel-time (track)
  (midimacs-time- midimacs-abs-time (midimacs-track-last-init-time track)))

(defun midimacs-track-set-event-at-beat (track beat event)
  (aset (midimacs-track-events track) beat event))

(defun midimacs-track-remove-event-at-beat (track time)
  (aset (midimacs-track-events track) beat nil))

(defun midimacs-track-events-at-beat (beat)
  (let ((event))
    (loop for track in midimacs-tracks
          if (setq event (midimacs-track-event-at-beat track beat))
          collect (list track event))))

(defun midimacs-check-track (track)
  (unless track
    (user-error "No track here")))

(defun midimacs-check-beat (beat)
  (unless (and beat (>= beat 0))
    (user-error "No beat here")))

(defun midimacs-remove-code ()
  (interactive)
  (let ((track (midimacs-current-track))
        (beat (midimacs-current-beat)))
    (midimacs-check-track track)
    (midimacs-check-beat beat)
    (midimacs-track-remove-event-at-beat track beat))
  (midimacs-draw))

(defun midimacs-remove-code-backward ()
  (interactive)
  (let ((track (midimacs-current-track))
        (beat (1- (midimacs-current-beat))))
    (midimacs-check-track track)
    (midimacs-check-beat beat)
    (midimacs-track-remove-event-at-beat track beat))
  (backward-char)
  (midimacs-draw))

(defun midimacs-add-code (c)
  (interactive "cCode name: ")
  (midimacs-add-code-init c t))

(defun midimacs-add-code-init (c do-init)
  (let ((track (midimacs-current-track))
        (beat (midimacs-current-beat))
        (code-name (midimacs-valid-letter c)))
    (midimacs-check-track track)
    (midimacs-check-beat beat)

    (midimacs-track-add-code-init-at-beat track beat c do-init))

  (midimacs-draw)
  (goto-char (1+ (point))))

(defun midimacs-track-add-code-init-at-beat (track beat c do-init)
  (midimacs-get-or-make-code code-name)
  (midimacs-track-set-event-at-beat track beat
                               (make-midimacs-event :code-name code-name
                                               :do-init do-init)))

(defun midimacs-get-or-make-code (code-name)
  (or (midimacs-get-code code-name)
      (let ((code (make-midimacs-code :name code-name
                                 :text (midimacs-code-template code-name))))
        (puthash code-name code midimacs-codes)
        code)))

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
      (split-window)
      (other-window 1))

    (switch-to-buffer buffer)))

(defun midimacs-current-track ()
  (interactive)
  (with-current-buffer (midimacs-buffer-seq)
    (save-excursion
      (midimacs-track-at-line (midimacs-current-line)))))

(defun midimacs-track-at-line (line)
  (when (>= line 2)
    (nth (- line 2) midimacs-tracks)))

(defun midimacs-beat-at-column (col)
  (when (>= col 2)
    (- col 2)))

(defun midimacs-current-beat ()
  (interactive)
  (with-current-buffer (midimacs-buffer-seq)
    (save-excursion
      (midimacs-beat-at-column (current-column)))))

(defun midimacs-valid-letter (c)
  (let ((vc (downcase (if (characterp c)
                          (string c)
                        c))))
    (unless (midimacs-string-member vc midimacs-letters)
      (user-error "Not a valid letter"))
    vc))

(defun midimacs-string-member (c s)
  (member (if (stringp c)
              (string-to-char c)
            c)
          (string-to-list s)))

(defun midimacs-current-line ()
  (save-restriction
    (widen)
    (save-excursion
      (beginning-of-line)
      (1+ (count-lines 1 (point))))))

(defun midimacs-set-buffer-seq ()
  (set-buffer (midimacs-buffer-seq-name)))

(defun midimacs-set-buffer-code (code)
  (set-buffer (midimacs-buffer-code-name (midimacs-code-name code))))

(defun midimacs-buffer-seq-name ()
  "*midimacs-seq*")

(defun midimacs-buffer-code-name (code-name)
  (concat "*midimacs-code-" code-name "*"))

(defun midimacs-buffer-seq ()
  (get-buffer (midimacs-buffer-seq-name)))

(defun midimacs-buffer-code (code-name)
  (get-buffer (midimacs-buffer-code-name code-name)))

(defun midimacs-event-code (track-event)
  (let ((code-name (midimacs-event-code-name track-event)))
    (midimacs-get-code code-name)))

(defun midimacs-event-string (track-event)
  (let* ((code (midimacs-event-code track-event))
         (do-init (midimacs-event-do-init track-event))
         (code-name (midimacs-code-name code)))
    (if do-init
        (upcase code-name)
      code-name)))

(defun midimacs-code-template (code-name)
  (concat
   "(midimacs-code \"" code-name "\"

 ;; init
 (lambda (song-time)

   nil)

 ;; run
 (lambda (song-time rel-time state)

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
  (message (concat "updated code " name)))

(cl-defun midimacs-note (channel pitch-raw duration-raw &optional (velocity 100) (off-velocity 0))
                                        ; TODO: validation

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
        ((eq midimacs-state 'stopped) (midimacs-play)))
  (midimacs-draw))

(defun midimacs-position-here ()
  (interactive)
  (let ((beat (midimacs-current-beat)))
    (midimacs-check-beat beat)
    (setq midimacs-song-time (make-midimacs-time :beat beat)))
  (midimacs-draw))

(defun midimacs-play-here ()
  (interactive)
  (midimacs-position-here)
  (when (eq midimacs-state 'stopped)
    (midimacs-play)))

(defun midimacs-play ()
  (setq midimacs-state 'playing)
  (setq midimacs-start-time-seconds (float-time))
  (setq midimacs-abs-time (make-midimacs-time))
  (midimacs-tick))

(defun midimacs-tick ()
  (when (eq midimacs-state 'playing)
    (midimacs-trigger-note-offs)
    (midimacs-trigger-events)
    (midimacs-incr-position)
    (midimacs-draw)
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
                  (run (midimacs-code-run code)))

             (when (and init
                        (midimacs-event-do-init event)
                        (eq (midimacs-time-tick midimacs-song-time) 0))
               (setf (midimacs-track-state track) (funcall init midimacs-song-time))
               (setf (midimacs-track-last-init-time track) midimacs-abs-time))

             (when run
               (setf (midimacs-track-state track)
                     (funcall run
                              midimacs-song-time
                              (midimacs-track-rel-time track)
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
  (setq midimacs-state 'stopped))

(defun midimacs-code-get-open-buffers ()
  (remove nil (loop for name being the hash-keys of midimacs-codes
                    collect (midimacs-buffer-code name))))

(defun midimacs-code-close-all-buffers ()
  (dolist (letter (string-to-list midimacs-letters))
    (let ((buffer-name (midimacs-buffer-code-name (string letter))))
      (when (get-buffer buffer-name)
        (kill-buffer buffer-name)))))

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
                         midimacs-tracks
                         midimacs-codes
                         midimacs-repeat-start
                         midimacs-repeat-end)))

(defun midimacs-open (filename)
  (interactive "fFind midimacs project: ")
  (let ((s (with-temp-buffer
             (insert-file-contents filename)
             (buffer-string))))

    (midimacs-unserialize-project s))
  (setq midimacs-filename filename)
  (midimacs-draw))

(defun midimacs-unserialize-project (s)
  (destructuring-bind (header
                       version
                       tracks
                       codes
                       repeat-start
                       repeat-end)
      (read s)

    (unless (equal header "midimacs project")
      (user-error "This doesn't appear to be a midimacs project"))
    (unless (equal version "v1")
      (user-error (concat "Unknown version: " version)))

    (midimacs-init)

    (setq midimacs-tracks tracks)
    (setq midimacs-codes codes)
    (setq midimacs-repeat-start repeat-start)
    (setq midimacs-repeat-end repeat-end)
    (setq midimacs-song-time midimacs-repeat-start)))

(defun midimacs-time-to-ticks (time)
  (+ (midimacs-time-tick time)
     (* (midimacs-time-beat time) midimacs-ticks-per-beat)))

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

(defun midimacs-time+ (a b)
  (let* ((abs-tick (+ (midimacs-time-tick a) (midimacs-time-tick b)))
         (abs-beat (+ (midimacs-time-beat a) (midimacs-time-beat b)))
         (tick (mod abs-tick midimacs-ticks-per-beat))
         (beat (+ abs-beat (floor (/ (float abs-tick) midimacs-ticks-per-beat)))))
    (make-midimacs-time :beat beat
                   :tick tick)))

(defun midimacs-time- (a b)
  (midimacs-time+ a (make-midimacs-time :beat (- (midimacs-time-beat b))
                              :tick (- (midimacs-time-tick b)))))

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

(defun midimacs-parse-time (s)
  (if (string-match (midimacs-time-regex) s)
      (let* ((beat-s (or (match-string 1 s) (match-string 2 s)))
             (frac-num-s (or (match-string 3 s) (match-string 5 s)))
             (frac-denom-s (or (match-string 4 s) (match-string 6 s)))
             (beat (if beat-s (string-to-number beat-s) 0))
             (frac-num (when frac-num-s (string-to-number frac-num-s)))
             (frac-denom (when frac-denom-s (string-to-number frac-denom-s)))
             (tick (if frac-num (midimacs-frac-to-tick frac-num frac-denom) 0)))
        (make-midimacs-time :beat beat :tick tick))
    (error (format "Couldn't parse time \"%s\"" s))))

(defun midimacs-frac-to-tick (num denom)
  (let ((tick (/ (float (* num midimacs-ticks-per-beat)) denom)))
    (unless (= (truncate tick) tick)
      (error (format "Invalid tick fraction %d/%d at %d ticks per beat (tick %f)"
                     num denom midimacs-ticks-per-beat tick)))
    (truncate tick)))

(defun midimacs-parse-pitch (s)
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
    (error (format "Couldn't parse pitch \"%s\"" s))))

(defun midimacs-sym-or-num-to-string (x)
  (cond ((symbolp x) (symbol-name x))
        ((numberp x) (number-to-string x))))

(defmacro midimacs-score (channel notes)
  (cons
   'progn
   (loop for (onset-sym pitch-sym dur-sym) in notes
         collect (let ((onset (midimacs-parse-time (midimacs-sym-or-num-to-string onset-sym)))
                       (pitch (midimacs-parse-pitch (symbol-name pitch-sym)))
                       (dur (midimacs-parse-time (midimacs-sym-or-num-to-string dur-sym))))
                   `(when (midimacs-time= rel-time ,onset)
                      (midimacs-note ,channel ,pitch ,dur))))))

(defun midimacs-visible-line-positions ()
  (save-excursion
    (let ((p (point))
          (col) (start) (end))
      (when (= (1- (line-number-at-pos))
               (count-lines (point-min) (point-max)))
        (setq col (current-column))
        (forward-line -1)
        (move-to-column col))

      (move-to-column (window-hscroll))
      (setq start (point))
      (setq col (current-column))
      (move-to-column (+ col (window-width)))
      (setq end (point))
      (loop for p from start to end
            collect p))))

(defun midimacs-visible-col-positions ()
  (save-excursion
    (let ((col (current-column))
          (lines (count-lines (point-min) (window-end))))
      (goto-char (window-start))
      (move-to-column col)
      (cons (point)
            (loop while (< (line-number-at-pos) lines)
                  collect (progn
                            (forward-line)
                            (move-to-column col)
                            (point)))))))

(defun midimacs-read-char-no-quit (q)
  (let* ((inhibit-quit t)
         (res (read-char q)))
    (if (= res 7)
        nil
      res)))

(defun midimacs-x-jump ()
  (interactive)
  (let* ((overlay-map (midimacs-x-jump-make-overlay-map))
         (jump-char (midimacs-read-char-no-quit "Jump to character: ")))
    (when jump-char
      (let* ((c-p-overlay (assoc jump-char overlay-map))
             (jump-point (car (cdr c-p-overlay))))
        (if jump-point
            (goto-char jump-point)
          (message "Invalid jump point"))))

    (loop for (c . (p . overlay)) in overlay-map
          do (delete-overlay overlay))))

(defun midimacs-x-jump-make-overlay-map ()
  (let* ((all-line-positions (midimacs-visible-line-positions))
         (col-positions (midimacs-visible-col-positions))

         (chars (nconc (loop for c from ?a to ?z collect c)
                       (loop for c from ?0 to ?9 collect c)
                       (loop for c from ?A to ?Z collect c)
                       (string-to-list "~`!@#$%^&*()_-+={[}]|\\:;\"'<,>.?/")))

         (line-positions (loop for p in all-line-positions
                               when (= (% p 3) (% (point) 3))
                               collect p))

         (positions (append col-positions line-positions)))

    (loop for (p c) in (mapcar* 'list positions chars)
          collect (cons c (cons p (midimacs-x-jump-make-overlay p (string c)))))))

(defun midimacs-x-jump-make-overlay (p c)
  (let ((ol (make-overlay p (1+ p))))
    (overlay-put ol 'face 'ace-jump-face-foreground)
    (overlay-put ol 'display c)
    ol))

(provide 'midimacs)
;;; midimacs.el ends here
