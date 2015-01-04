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

(define-derived-mode daw-seq-mode special-mode "daw-seq-mode"
  (daw-seq-define-letter-keys)
  (define-key daw-seq-mode-map (kbd "[") 'daw-set-repeat-start)
  (define-key daw-seq-mode-map (kbd "]") 'daw-set-repeat-end)
  (define-key daw-seq-mode-map (kbd "C-c a") 'daw-add-track)
  (define-key daw-seq-mode-map (kbd "C-d") 'daw-remove-code)
  (define-key daw-seq-mode-map (kbd "DEL") 'daw-remove-code-backward)
  (define-key daw-seq-mode-map (kbd "RET") 'daw-seq-enter)
  (define-key daw-seq-mode-map (kbd "C-w") 'daw-seq-kill)
  (define-key daw-seq-mode-map (kbd "C-y") 'daw-seq-yank)
  (define-key daw-seq-mode-map (kbd "C-x SPC") 'daw-toggle-play)
  (define-key daw-seq-mode-map (kbd "SPC") 'daw-toggle-play)
  (define-key daw-seq-mode-map (kbd "M-SPC") 'daw-play-here)
  (define-key daw-seq-mode-map (kbd "C-<return>") 'daw-position-here)
  (define-key daw-seq-mode-map (kbd "C-x C-s") 'daw-save)
  (define-key daw-seq-mode-map (kbd "C-x C-w") 'daw-save-as)
  (define-key daw-seq-mode-map (kbd "C-x C-f") 'daw-open)
  (define-key daw-seq-mode-map (kbd "C-c SPC") 'daw-x-jump)
  (setq transient-mark-mode nil)
  (setq truncate-lines t))

(define-derived-mode daw-code-mode emacs-lisp-mode "daw-code-mode"
  (define-key daw-code-mode-map (kbd "C-x SPC") 'daw-toggle-play)
  (define-key daw-code-mode-map (kbd "C-x C-s") 'daw-save)
  (define-key daw-code-mode-map (kbd "C-x C-w") 'daw-save-as)
  (define-key daw-code-mode-map (kbd "C-x C-f") 'daw-open)
  (define-key daw-code-mode-map (kbd "C-c C-c") 'daw-code-update))

;;;###autoload
(defun daw () "Start DAW"
  (interactive)
  (switch-to-buffer (daw-buffer-seq-name))
  (daw-seq-mode)
  (daw-init))

(defcustom daw-length 256
  "number of beats"
  :group 'daw)

(defcustom daw-bpm 120
  "beats per minute"
  :group 'daw)

(defcustom daw-ticks-per-beat 24
  "number of ticks per beat"
  :group 'daw)

(defconst daw-letters "abcdefghijklmnopqrstuvwxyz")
(defconst daw-pitch-numbers '(("c" . 0)
                              ("d" . 2)
                              ("e" . 4)
                              ("f" . 5)
                              ("g" . 7)
                              ("a" . 9)
                              ("b" . 11)))
(defconst daw-accidental-numbers '((nil . 0)
                                   ("b" . -1)
                                   ("s" . +1)))

(defvar daw-tracks '())
(defvar daw-codes nil)
(defvar daw-repeat-start nil)
(defvar daw-repeat-end daw-length)
(defvar daw-repeat-end nil)
(defvar daw-abs-time nil)
(defvar daw-song-time nil)
(defvar daw-state 'stopped)
(defvar daw-start-time-seconds nil)
(defvar daw-filename nil)
(defvar daw-amidicat-proc nil)
(defvar daw-scheduled-note-offs nil)
(defvar daw-selection nil)

;(defface daw-seq-region-face '((t :inherit 'region)) "Face for DAW sequencer regions" :group 'daw-faces)

(defstruct daw-event
  code-name
  do-init)

(defstruct daw-code
  name
  text
  init
  run)

(defstruct daw-track
  name
  events
  state
  (last-init-time (make-daw-time)))

(defstruct daw-midi-message
  status
  (data1 nil)
  (data2 nil))

(defstruct daw-time
  (beat 0)
  (tick 0))

(defun daw-seq-define-letter-keys ()
  (dolist (c (string-to-list daw-letters))
    (lexical-let ((s (string c)))

      (define-key daw-seq-mode-map (kbd s)
        (lambda ()
          (interactive)
          (daw-add-code-init s nil)))

      (define-key daw-seq-mode-map (kbd (upcase s))
        (lambda ()
          (interactive)
          (daw-add-code-init s t))))))

(defun daw-init ()
  (setq daw-tracks '())
  (setq daw-codes (make-hash-table :test 'equal))
  (setq daw-song-time (make-daw-time))
  (setq daw-abs-time (make-daw-time))
  (setq daw-state 'stopped)
  (setq daw-start-time-seconds nil)
  (setq daw-filename nil)
  (setq daw-scheduled-note-offs (daw-make-scheduled-note-offs-heap))
  (setq daw-repeat-start (make-daw-time))
  (setq daw-repeat-end (make-daw-time :beat daw-length))
  (setq daw-selection nil)

  (daw-code-close-all-buffers)
  (daw-amidicat-proc-init)
  (daw-draw))

(defun daw-amidicat-proc-init ()
  (interactive)
  (let ((buffer-name (daw-amidicat-buffer-name))
        (process-name "daw-amidicat"))
    (when (get-buffer buffer-name)
      (when (get-process process-name)
          (progn
            (daw-midi-flush-note-offs)
            (delete-process process-name)))
      (kill-buffer buffer-name))

    (setq daw-amidicat-proc
          (start-process process-name buffer-name
                         "amidicat" "--hex" "--port" "129:0" "--noread"))
    (set-process-filter daw-amidicat-proc 'daw-amidicat-read)))

(defun daw-draw ()
  (interactive)
  (with-current-buffer (daw-buffer-seq)
    (let ((inhibit-read-only t)
          (p (point)))
      (erase-buffer)
      (daw-draw-top-bar)
      (daw-draw-tracks)
      (goto-char p))))

(defun daw-draw-top-bar ()
  (let* ((p 0)
         (space (string-to-char " "))
         (repeat-start-col (+ (daw-time-beat daw-repeat-start) 1))
         (repeat-end-col (+ (daw-time-beat daw-repeat-end) 2))
         (position-col (+ (daw-time-beat daw-song-time) 2))
         (chars `((,repeat-start-col . "[")
                  (,repeat-end-col . "]")
                  (,position-col . ,(daw-play-symbol))))
         (sorted-chars (sort chars (lambda (a b)
                                     (< (car a) (car b))))))

    (loop for (col . s) in sorted-chars do
          (let ((spaces (- col p)))
            (insert (make-string spaces space))
            (insert s)
            (setq p (+ p spaces (length s)))))

    (insert (make-string (max 0 (+ (- daw-length p) 2)) space)))

  (insert "\n"))

(defun daw-play-symbol ()
  (cond ((eq daw-state 'playing) "▶")
        ((eq daw-state 'stopped) "◾")))

(defun daw-draw-tracks ()
  (dolist (track daw-tracks)
    (daw-draw-track track)))

(defun daw-draw-track (track)
  (insert (daw-track-name track))
  (insert " ")
  (daw-draw-track-events track)
  (insert "\n"))

(defun daw-draw-track-events (track)
  (let ((track-events (daw-track-events track)))
    (dotimes (i daw-length)
      (let* ((track-event (elt track-events i)))
        (daw-draw-track-event track-event)))))

(defun daw-daw-track-event (track-event)
  (insert (if (eq track-event nil)
              "-"
            (daw-event-string track-event))))

(defun daw-seq-enter ()
  (interactive)
  (let* ((track (daw-current-track))
         (beat (daw-current-beat))
         (time (make-daw-time :beat beat))
         (code (daw-track-code-at-beat track (daw-time-beat time))))
    (when (and track beat code)
      (daw-code-open-window code))))

(defun daw-seq-kill ()
  (interactive)
  (destructuring-bind (col-start line-start col-end line-end)
      (daw-selected-rect)
    (unless (and (daw-beat-at-column col-start)
                 (daw-track-at-line line-start)
                 (daw-beat-at-column col-end)
                 (daw-track-at-line line-end))
      (user-error "Cannot kill region without track or beat"))

    (setq daw-selection
          (loop for line from line-start to line-end
                collect (let ((track (daw-track-at-line line)))
                          (loop for col from col-start to col-end
                                collect (let* ((beat (daw-beat-at-column col))
                                               (event (daw-track-event-at-beat track beat)))
                                          (daw-track-remove-event-at-beat track beat)
                                          event))))))
  (goto-char (mark))
  (daw-draw))

(defun daw-column (pos)
  (save-excursion
    (goto-char pos)
    (current-column)))

(defun daw-selected-rect ()
  (let ((col-mark (daw-column (mark)))
        (line-mark (count-lines 1 (mark)))
        (col-point (daw-column (point)))
        (line-point (count-lines 1 (point))))
    (list (min col-mark col-point)
          (min line-mark line-point)
          (max col-mark col-point)
          (max line-mark line-point))))

(defun daw-set-repeat-start ()
  (interactive)
  (save-excursion
    (forward-char)
    (let ((new-repeat-start (make-daw-time :beat (daw-current-beat))))
      (daw-check-acceptable-repeat new-repeat-start daw-repeat-end)
      (setq daw-repeat-start new-repeat-start)))
  (daw-draw))

(defun daw-seq-yank ()
  (interactive)
  (when daw-selection
    (let* ((col-start (daw-column (point)))
           (line-start (count-lines 1 (point)))
           (cols (length (car daw-selection)))
           (lines (length daw-selection)))

      (loop for line below lines
            do (let ((track (daw-track-at-line (+ line-start line))))
                 (when track
                   (loop for col below cols
                         do (let ((beat (daw-beat-at-column (+ col-start col))))
                              (when beat
                                (daw-track-set-event-at-beat
                                 track beat (nth col (nth line daw-selection)))))))))))
  (daw-draw))

(defun daw-set-repeat-end ()
  (interactive)
  (let ((new-repeat-end (make-daw-time :beat (daw-current-beat))))
    (daw-check-acceptable-repeat daw-repeat-start new-repeat-end)
    (setq daw-repeat-end new-repeat-end))
  (daw-draw))

(defun daw-check-acceptable-repeat (start end)
  (let ((beats (- (daw-time-beat start) (daw-time-beat end))))
    (when (= beats 0)
      (user-error "repeat start and repeat end must be different"))))

(defun daw-add-track ()
  (interactive)
  (let ((track (make-daw-track :name (daw-next-track-name)
                               :events (make-vector daw-length nil)
                               :state nil)))
    (setq daw-tracks (append daw-tracks (list track))))

  (daw-draw))

(defun daw-next-track-name ()
  (string (elt daw-letters (length daw-tracks))))

(defun daw-track-event-at-beat (track beat)
  (elt (daw-track-events track) beat))

(defun daw-track-code-at-beat (track beat)
  (let ((event (daw-track-event-at-beat track beat)))
    (when event
      (daw-event-code event))))

(defun daw-track-rel-time (track)
  (daw-time- daw-abs-time (daw-track-last-init-time track)))

(defun daw-track-set-event-at-beat (track beat event)
  (aset (daw-track-events track) beat event))

(defun daw-track-remove-event-at-beat (track time)
  (aset (daw-track-events track) beat nil))

(defun daw-track-events-at-beat (beat)
  (let ((event))
    (loop for track in daw-tracks
          if (setq event (daw-track-event-at-beat track beat))
          collect (list track event))))

(defun daw-check-track (track)
  (unless track
    (user-error "No track here")))

(defun daw-check-beat (beat)
  (unless (and beat (>= beat 0))
    (user-error "No beat here")))

(defun daw-remove-code ()
  (interactive)
  (let ((track (daw-current-track))
        (beat (daw-current-beat)))
    (daw-check-track track)
    (daw-check-beat beat)
    (daw-track-remove-event-at-beat track beat))
  (daw-draw))

(defun daw-remove-code-backward ()
  (interactive)
  (let ((track (daw-current-track))
        (beat (1- (daw-current-beat))))
    (daw-check-track track)
    (daw-check-beat beat)
    (daw-track-remove-event-at-beat track beat))
  (backward-char)
  (daw-draw))

(defun daw-add-code (c)
  (interactive "cCode name: ")
  (daw-add-code-init c t))

(defun daw-add-code-init (c do-init)
  (let ((track (daw-current-track))
        (beat (daw-current-beat))
        (code-name (daw-valid-letter c)))
    (daw-check-track track)
    (daw-check-beat beat)

    (daw-track-add-code-init-at-beat track beat c do-init))

  (daw-draw)
  (goto-char (1+ (point))))

(defun daw-track-add-code-init-at-beat (track beat c do-init)
  (daw-get-or-make-code code-name)
  (daw-track-set-event-at-beat track beat
                               (make-daw-event :code-name code-name
                                               :do-init do-init)))

(defun daw-get-or-make-code (code-name)
  (or (daw-get-code code-name)
      (let ((code (make-daw-code :name code-name
                                 :text (daw-code-template code-name))))
        (puthash code-name code daw-codes)
        code)))

(defun daw-get-code (code-name)
  (gethash code-name daw-codes))

(defun daw-code-open-window (code)
  (let* ((buffer-name (daw-buffer-code-name (daw-code-name code)))
         (buffer-existed (get-buffer buffer-name))
         (visible-window (get-buffer-window buffer-name))
         (buffer (get-buffer-create buffer-name)))

    (unless buffer-existed
      (with-current-buffer buffer
        (insert (daw-code-text code))
        (beginning-of-buffer)
        (daw-code-mode)))

    (if visible-window
        (select-window visible-window)
      (split-window)
      (other-window 1))

    (switch-to-buffer buffer)))

(defun daw-current-track ()
  (interactive)
  (with-current-buffer (daw-buffer-seq)
    (save-excursion
      (daw-track-at-line (daw-current-line)))))

(defun daw-track-at-line (line)
  (when (>= line 2)
    (nth (- line 2) daw-tracks)))

(defun daw-beat-at-column (col)
  (when (>= col 2)
    (- col 2)))

(defun daw-current-beat ()
  (interactive)
  (with-current-buffer (daw-buffer-seq)
    (save-excursion
      (daw-beat-at-column (current-column)))))

(defun daw-valid-letter (c)
  (let ((vc (downcase (if (characterp c)
                          (string c)
                        c))))
    (unless (daw-string-member vc daw-letters)
      (user-error "Not a valid letter"))
    vc))

(defun daw-string-member (c s)
  (member (if (stringp c)
              (string-to-char c)
            c)
          (string-to-list s)))

(defun daw-current-line ()
  (save-restriction
    (widen)
    (save-excursion
      (beginning-of-line)
      (1+ (count-lines 1 (point))))))

(defun daw-set-buffer-seq ()
  (set-buffer (daw-buffer-seq-name)))

(defun daw-set-buffer-code (code)
  (set-buffer (daw-buffer-code-name (daw-code-name code))))

(defun daw-buffer-seq-name ()
  "*daw-seq*")

(defun daw-buffer-code-name (code-name)
  (concat "*daw-code-" code-name "*"))

(defun daw-buffer-seq ()
  (get-buffer (daw-buffer-seq-name)))

(defun daw-buffer-code (code-name)
  (get-buffer (daw-buffer-code-name code-name)))

(defun daw-event-code (track-event)
  (let ((code-name (daw-event-code-name track-event)))
    (daw-get-code code-name)))

(defun daw-event-string (track-event)
  (let* ((code (daw-event-code track-event))
         (do-init (daw-event-do-init track-event))
         (code-name (daw-code-name code)))
    (if do-init
        (upcase code-name)
      code-name)))

(defun daw-code-template (code-name)
  (concat
   "(daw-code \"" code-name "\"

 ;; init
 (lambda (song-time)

   nil)

 ;; run
 (lambda (song-time rel-time state)

   state)

 )
"))

(defun daw-code-update ()
  (interactive)
  (eval-buffer))

(defun daw-code (name init run)
  (let ((code (daw-get-code name)))
    (setf (daw-code-init code) init
          (daw-code-run code) run
          (daw-code-text code) (buffer-string)))
  (message (concat "updated code " name)))

(cl-defun daw-note (channel pitch-raw duration-raw &optional (velocity 100) (off-velocity 0))
                                        ; TODO: validation

  (setq pitch (cond ((symbolp pitch-raw) (daw-parse-pitch (symbol-name pitch-raw)))
                    ((stringp pitch-raw) (daw-parse-pitch pitch-raw))
                    (t pitch-raw)))

  (setq duration (cond ((symbolp duration-raw) (daw-parse-time (symbol-name duration-raw)))
                       ((stringp duration-raw) (daw-parse-time duration-raw))
                       ((numberp duration-raw)
                        (make-daw-time :beat (floor (/ duration-raw daw-ticks-per-beat))
                                       :tick (mod duration-raw daw-ticks-per-beat)))
                       (t duration-raw)))

  (daw-midi-schedule-note-off (daw-time+ daw-abs-time duration)
                              (daw-midi-message-note-off channel pitch off-velocity))
  (daw-midi-execute (daw-midi-message-note-on channel pitch velocity)))

(defun daw-midi-message-note-on (channel pitch velocity)
  (make-daw-midi-message :status (+ #x90 channel)
                         :data1 pitch
                         :data2 velocity))

(defun daw-midi-message-note-off (channel pitch velocity)
  (make-daw-midi-message :status (+ #x80 channel)
                         :data1 pitch
                         :data2 velocity))

(defun daw-midi-message-program-change (channel program)
  (make-daw-midi-message :status (+ #xC0 channel)
                         :data1 program))

(defun daw-program-change (channel program)
  (daw-midi-execute (daw-midi-message-program-change channel program)))

(defun daw-midi-schedule-note-off (abs-time message)
  (heap-add daw-scheduled-note-offs (list abs-time message)))

(defun daw-midi-execute (message)
  (process-send-string daw-amidicat-proc (daw-midi-serialize message)))

(defun daw-midi-serialize (message)
  (let* ((status (daw-midi-message-status message))
         (data1 (daw-midi-message-data1 message))
         (data2 (daw-midi-message-data2 message))
         (status-string (format "%02X" status))
         (data1-string (if data1 (format " %02X" data1) ""))
         (data2-string (if (and data1 data2) (format " %02X" data2) "")))
    (concat status-string data1-string data2-string "\n")))

(defun daw-midi-flush-note-offs ()
  (let ((el))
    (loop while (setq el (heap-delete-root daw-scheduled-note-offs))
          do (daw-midi-execute (nth 1 el)))))

(defun daw-amidicat-buffer-name ()
  "*daw-amidicat*")

(defun daw-amidicat-read (proc string)
                                        ; TODO
  (message string))

(defun daw-toggle-play ()
  (interactive)
  (cond ((eq daw-state 'playing) (daw-stop))
        ((eq daw-state 'stopped) (daw-play)))
  (daw-draw))

(defun daw-position-here ()
  (interactive)
  (let ((beat (daw-current-beat)))
    (daw-check-beat beat)
    (setq daw-song-time (make-daw-time :beat beat)))
  (daw-draw))

(defun daw-play-here ()
  (interactive)
  (daw-position-here)
  (when (eq daw-state 'stopped)
    (daw-play)))

(defun daw-play ()
  (setq daw-state 'playing)
  (setq daw-start-time-seconds (float-time))
  (setq daw-abs-time (make-daw-time))
  (daw-tick))

(defun daw-tick ()
  (daw-trigger-note-offs)
  (daw-trigger-events)
  (daw-incr-position)
  (daw-draw)
  (when (eq daw-state 'playing)
    (run-at-time (daw-wait-time) nil 'daw-tick)))

(defun daw-incr-position ()
  (let ((1-tick (make-daw-time :tick 1)))
    (setq daw-abs-time (daw-time+ daw-abs-time 1-tick))
    (setq daw-song-time (daw-time+ daw-song-time 1-tick))

    (when (daw-time= daw-song-time daw-repeat-end)
      (setq daw-song-time daw-repeat-start))))

(defun daw-trigger-note-offs ()
  (let ((note-offs (daw-delete-message-heap-until daw-scheduled-note-offs daw-abs-time)))
    (dolist (note-off note-offs)
      (daw-midi-execute note-off))))

(defun daw-delete-message-heap-until (heap pos)
  (let ((el))
    (loop while (and (setq el (heap-root heap))
                     (daw-time<= (nth 0 el) pos))
          collect (nth 1 (heap-delete-root heap)))))

(defun daw-trigger-events ()
  (loop for (track event) in (daw-track-events-at-time daw-song-time)
        do (let* ((code (daw-event-code event))
                  (init (daw-code-init code))
                  (run (daw-code-run code)))

             (when (and init
                        (daw-event-do-init event)
                        (eq (daw-time-tick daw-song-time) 0))
               (setf (daw-track-state track) (funcall init daw-song-time))
               (setf (daw-track-last-init-time track) daw-abs-time))

             (when run
               (setf (daw-track-state track)
                     (funcall run
                              daw-song-time
                              (daw-track-rel-time track)
                              (daw-track-state track)))))))

(defun daw-wait-time ()
  (let* ((tick-sec (/ 60.0 daw-bpm daw-ticks-per-beat))
         (expected-time-sec (+ daw-start-time-seconds
                               (* tick-sec (daw-time-to-ticks daw-abs-time))))
         (now (float-time))
         (drift (- now expected-time-sec)))
    (max 0 (- tick-sec drift))))

(defun daw-stop ()
  (daw-midi-flush-note-offs)
  (setq daw-state 'stopped))

(defun daw-code-get-open-buffers ()
  (remove nil (loop for name being the hash-keys of daw-codes
                    collect (daw-buffer-code name))))

(defun daw-code-close-all-buffers ()
  (dolist (letter (string-to-list daw-letters))
    (let ((buffer-name (daw-buffer-code-name (string letter))))
      (when (get-buffer buffer-name)
        (kill-buffer buffer-name)))))

(defun daw-code-update-open-buffers ()
  (dolist (buffer (daw-code-get-open-buffers))
    (with-current-buffer buffer
      (daw-code-update))))

(defun daw-save-as (filename)
  (interactive "FWrite DAW project: ")
  (setq daw-filename filename)
  (daw-save))

(defun daw-save ()
  (interactive)
  (daw-code-update-open-buffers)
  (let ((filename (or daw-filename
                      (read-file-name "Write DAW project: "))))
    (with-temp-buffer
      (insert (daw-serialize-project))
      (write-file filename))
    (setq daw-filename filename)
    (message (concat "Wrote " filename))))

(defun daw-serialize-project ()
  (prin1-to-string (list "daw project"
                         "v1"
                         daw-tracks
                         daw-codes
                         daw-repeat-start
                         daw-repeat-end)))

(defun daw-open (filename)
  (interactive "fFind DAW project: ")
  (let ((s (with-temp-buffer
             (insert-file-contents filename)
             (buffer-string))))

    (daw-unserialize-project s))
  (setq daw-filename filename)
  (daw-draw))

(defun daw-unserialize-project (s)
  (destructuring-bind (header
                       version
                       tracks
                       codes
                       repeat-start
                       repeat-end)
      (read s)

    (unless (equal header "daw project")
      (user-error "This doesn't appear to be a daw project"))
    (unless (equal version "v1")
      (user-error (concat "Unknown version: " version)))

    (daw-init)

    (setq daw-tracks tracks)
    (setq daw-codes codes)
    (setq daw-repeat-start repeat-start)
    (setq daw-repeat-end repeat-end)
    (setq daw-song-time daw-repeat-start)))

(defun daw-time-to-ticks (time)
  (+ (daw-time-tick time)
     (* (daw-time-beat time) daw-ticks-per-beat)))

(defun daw-time< (a b)
  (let ((beat-a (daw-time-beat a))
        (beat-b (daw-time-beat b))
        (tick-a (daw-time-tick a))
        (tick-b (daw-time-tick b)))

    (cond ((< beat-a beat-b) t)
          ((and (= beat-a beat-b) (< tick-a tick-b)) t))))

(defun daw-time= (a b)
  (and (= (daw-time-beat a) (daw-time-beat b))
       (= (daw-time-tick a) (daw-time-tick b))))

(defun daw-time<= (a b)
  (or (daw-time= a b) (daw-time< a b)))

(defun daw-time> (a b)
  (not (daw-time<= a b)))

(defun daw-time+ (a b)
  (let* ((abs-tick (+ (daw-time-tick a) (daw-time-tick b)))
         (abs-beat (+ (daw-time-beat a) (daw-time-beat b)))
         (tick (mod abs-tick daw-ticks-per-beat))
         (beat (+ abs-beat (floor (/ (float abs-tick) daw-ticks-per-beat)))))
    (make-daw-time :beat beat
                   :tick tick)))

(defun daw-time- (a b)
  (daw-time+ a (make-daw-time :beat (- (daw-time-beat b))
                              :tick (- (daw-time-tick b)))))

(defun daw-make-scheduled-note-offs-heap ()
  (make-heap (lambda (a b) (daw-time< (nth 0 a) (nth 0 b)))))

(defun daw-time-regex ()
  (let ((beat-regex "\\([0-9]+\\)")
        (frac-regex "\\([0-9]+\\)/\\([0-9]+\\)"))
    (concat "^\\(?:"
            beat-regex
            "\\|"
            beat-regex "\\+" frac-regex
            "\\|"
            frac-regex
            "\\)$")))

(defun daw-parse-time (s)
  (if (string-match (daw-time-regex) s)
      (let* ((beat-s (or (match-string 1 s) (match-string 2 s)))
             (frac-num-s (or (match-string 3 s) (match-string 5 s)))
             (frac-denom-s (or (match-string 4 s) (match-string 6 s)))
             (beat (if beat-s (string-to-number beat-s) 0))
             (frac-num (when frac-num-s (string-to-number frac-num-s)))
             (frac-denom (when frac-denom-s (string-to-number frac-denom-s)))
             (tick (if frac-num (daw-frac-to-tick frac-num frac-denom) 0)))
        (make-daw-time :beat beat :tick tick))
    (error (format "Couldn't parse time \"%s\"" s))))

(defun daw-frac-to-tick (num denom)
  (let ((tick (/ (float (* num daw-ticks-per-beat)) denom)))
    (unless (= (truncate tick) tick)
      (error (format "Invalid tick fraction %d/%d at %d ticks per beat (tick %f)"
                     num denom daw-ticks-per-beat tick)))
    (truncate tick)))

(defun daw-parse-pitch (s)
  (if (string-match "^\\([a-gA-G]\\)\\([sb]\\)?\\(-?[0-9]\\)$" s)
      (let* ((base (downcase (match-string 1 s)))
             (accidental (match-string 2 s))
             (octave (string-to-number (match-string 3 s)))
             (pitch (+ (* 12 (+ octave 1))
                       (+ (cdr (assoc base daw-pitch-numbers))
                          (cdr (assoc accidental daw-accidental-numbers))))))
        (unless (and (>= pitch 0)
                     (<= pitch 127))
          (error (format "Pitch \"%s\" (%d) is out of range 0 <= x <= 127" s pitch)))
        pitch)
    (error (format "Couldn't parse pitch \"%s\"" s))))

(defun daw-sym-or-num-to-string (x)
  (cond ((symbolp x) (symbol-name x))
        ((numberp x) (number-to-string x))))

(defmacro daw-score (channel notes)
  (cons
   'progn
   (loop for (onset-sym pitch-sym dur-sym) in notes
         collect (let ((onset (daw-parse-time (daw-sym-or-num-to-string onset-sym)))
                       (pitch (daw-parse-pitch (symbol-name pitch-sym)))
                       (dur (daw-parse-time (daw-sym-or-num-to-string dur-sym))))
                   `(when (daw-time= rel-time ,onset)
                      (daw-note ,channel ,pitch ,dur))))))

(defun daw-visible-line-positions ()
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

(defun daw-visible-col-positions ()
  (save-excursion
    (let ((col (current-column))
          (lines (count-lines (window-start) (window-end))))
      (goto-char (window-start))
      (move-to-column col)
      (cons (point)
            (loop while (< (line-number-at-pos) lines)
                  collect (progn
                            (forward-line)
                            (move-to-column col)
                            (point)))))))

(defun daw-read-char-no-quit (q)
  (let* ((inhibit-quit t)
         (res (read-char q)))
    (if (= res 7)
        nil
      res)))

(defun daw-x-jump ()
  (interactive)
  (let* ((overlay-map (daw-x-jump-make-overlay-map))
         (jump-char (daw-read-char-no-quit "Jump to character: ")))
    (when jump-char
      (let* ((c-p-overlay (assoc jump-char overlay-map))
             (jump-point (car (cdr c-p-overlay))))
        (if jump-point
            (goto-char jump-point)
          (message "Invalid jump point"))))

    (loop for (c . (p . overlay)) in overlay-map
          do (delete-overlay overlay))))

(defun daw-x-jump-make-overlay-map ()
  (let* ((all-line-positions (daw-visible-line-positions))
         (col-positions (daw-visible-col-positions))

         (chars (nconc (loop for c from ?a to ?z collect c)
                       (loop for c from ?0 to ?9 collect c)
                       (loop for c from ?A to ?Z collect c)
                       (string-to-list "~`!@#$%^&*()_-+={[}]|\\:;\"'<,>.?/")))

         (line-positions (loop for p in all-line-positions
                               when (= (% p 3) (% (point) 3))
                               collect p))

         (positions (append col-positions line-positions)))

    (loop for (p c) in (mapcar* 'list positions chars)
          collect (cons c (cons p (daw-x-jump-make-overlay p (string c)))))))

(defun daw-x-jump-make-overlay (p c)
  (let ((ol (make-overlay p (1+ p))))
    (overlay-put ol 'face 'ace-jump-face-foreground)
    (overlay-put ol 'display c)
    ol))

(provide 'daw)
;;; daw.el ends here
