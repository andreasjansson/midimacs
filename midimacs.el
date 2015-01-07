;; TODO:
;; handle parse errors better!
;; be more forgiving but use font-lock to show where the problems are.
;; fix x-jump
;;
;; set tempo by tapping!
;;
;; midi event history per channel!

;; BUGS:
;; cannot save when code doesn't parse
;; when midi server goes away we die

(eval-when-compile
  (require 'cl)
  (require 'heap)
  (require 'picture))

(define-derived-mode midimacs-seq-mode fundamental-mode "midimacs-seq-mode"
  (midimacs-extend-picture-mode)
  (local-set-key (kbd "C-x C-[") 'midimacs-set-repeat-start)
  (local-set-key (kbd "C-x C-]") 'midimacs-set-repeat-end)
  (local-set-key (kbd "RET") 'midimacs-seq-enter)
  (local-set-key (kbd "C-x SPC") 'midimacs-toggle-play)
  (local-set-key (kbd "M-SPC") 'midimacs-toggle-play)
  (local-set-key (kbd "C-M-SPC") 'midimacs-play-here)
  (local-set-key (kbd "C-<return>") 'midimacs-position-here)
  (local-set-key (kbd "C-x C-s") 'midimacs-save)
  (local-set-key (kbd "C-x C-w") 'midimacs-save-as)
  (local-set-key (kbd "C-x C-f") 'midimacs-open)
  (local-set-key (kbd "C-c SPC") 'midimacs-x-jump)
  (setq-local after-change-functions '(midimacs-seq-after-change))
  (setq-local transient-mark-mode nil)
  (setq-local font-lock-defaults `(((,(midimacs-bad-track-regex) . font-lock-warning-face))))
  (setq truncate-lines t))

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
(defconst midimacs-left-bar-length 4)
(defconst midimacs-top-bar-height 1)
(defconst midimacs-track-char ?>)
(defconst midimacs-sustain-char ?.)
(defconst midimacs-space-char ? )

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

(defface midimacs-error-face
  '((t (:background "red")))
  "midimacs error face"
  :group 'midimacs)

(defface midimacs-x-jump-face-background
  '((t (:foreground "gray40")))
  "Face for background of midimacs x-jump"
  :group 'midimacs)

(defface midimacs-x-jump-face-foreground
  '((t (:foreground "red" :underline nil)))
  "Face for foreground of midimacs x-jump"
  :group 'midimacs)

(defstruct midimacs-event
  code-name
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
        (current nil))
    (loop for c being the elements of chars
          using (index i)
          collect (cond ((= c midimacs-space-char)
                         (setq current nil)
                         nil)
                        ((= c midimacs-sustain-char)
                         (when current
                             (make-midimacs-event :code-name current
                                                  :do-init nil)))
                        (t
                         (setq current c)
                         (make-midimacs-event :code-name c
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

(defun midimacs-draw (contents)
  (with-current-buffer (midimacs-buffer-seq)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (midimacs-draw-top-bar)
      (insert "\n")
      (insert contents)
      (goto-char (point-min)))))

(defun midimacs-redraw-top-bar ()
  (with-current-buffer (midimacs-buffer-seq)
    (let ((inhibit-read-only t)
          (p (point)))
      (goto-char (point-min))
      (delete-region (point) (line-end-position))
      (midimacs-draw-top-bar)
      (goto-char p))))

(defun midimacs-draw-top-bar ()
  (let* ((pad ?.)
         (repeat-start-col (+ (midimacs-time-beat midimacs-repeat-start) (1- midimacs-left-bar-length)))
         (repeat-end-col (+ (midimacs-time-beat midimacs-repeat-end) midimacs-left-bar-length))
         (position-col (+ (midimacs-time-beat midimacs-song-time) midimacs-left-bar-length))

         (chars (list (cons repeat-start-col "[")
                      (cons repeat-end-col "]")
                      (cons position-col (midimacs-play-symbol))))

         (sorted-chars (sort chars (lambda (a b)
                                     (< (car a) (car b))))))

    (insert (make-string midimacs-left-bar-length (string-to-char " ")))

    (loop for (col . s) in sorted-chars do
          (let ((spaces (- col (1- (point)))))
            (if (= spaces -1)
                (progn
                  (delete-backward-char 1)
                  (when (> (point) midimacs-left-bar-length)
                    (setq s "#")))
              (insert (make-string spaces pad)))
            (insert s)))

    (insert (make-string (max 0 (+ (- midimacs-length (point)) midimacs-left-bar-length)) pad)))
  (put-text-property (point-min) (point) 'read-only t))

(defun midimacs-play-symbol ()
  (cond ((eq midimacs-state 'playing) "▶")
        ((eq midimacs-state 'stopped) "◾")))

(defun midimacs-current-track ()
  (with-current-buffer (midimacs-buffer-seq)
    (save-excursion
      (beginning-of-line)
      (midimacs-string-is-track (buffer-substring (point) (line-end-position))))))

(defun midimacs-current-beat ()
  (with-current-buffer (midimacs-buffer-seq)
    (midimacs-beat-at-column (current-column))))

(defun midimacs-seq-enter ()
  (interactive)
  (let ((track (midimacs-current-track))
        (beat (midimacs-current-beat)))
    (when (and track beat)
      (midimacs-code-open-window (midimacs-track-code-at-beat track beat)))))

(defun midimacs-set-repeat-start ()
  (interactive)
  (save-excursion
    (forward-char)
    (let ((new-repeat-start (make-midimacs-time :beat (midimacs-current-beat))))
      (midimacs-check-acceptable-repeat new-repeat-start midimacs-repeat-end)
      (setq midimacs-repeat-start new-repeat-start)))
  (midimacs-redraw-top-bar))

(defun midimacs-set-repeat-end ()
  (interactive)
  (let ((new-repeat-end (make-midimacs-time :beat (midimacs-current-beat))))
    (midimacs-check-acceptable-repeat midimacs-repeat-start new-repeat-end)
    (setq midimacs-repeat-end new-repeat-end))
  (midimacs-redraw-top-bar))

(defun midimacs-check-acceptable-repeat (start end)
  (let ((beats (- (midimacs-time-beat start) (midimacs-time-beat end))))
    (when (= beats 0)
      (user-error "repeat start and repeat end must be different"))))

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

(defun midimacs-event-string (track-event)
  (let* ((code (midimacs-event-code track-event))
         (do-init (midimacs-event-do-init track-event))
         (code-name (midimacs-code-name code)))
    (if do-init
        (upcase code-name)
      code-name)))

(defun midimacs-code-template (code-name)
  (concat
   "(midimacs-code ?" (string code-name) "

 ;; init
 (lambda (channel song-time)

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
        ((eq midimacs-state 'stopped) (midimacs-play))))

(defun midimacs-position-here ()
  (interactive)
  (let ((beat (midimacs-current-beat)))
    (midimacs-check-beat beat)
    (setq midimacs-song-time (make-midimacs-time :beat beat)))
  (midimacs-redraw-top-bar))

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

    ;; only update when we have to
    (when (= (midimacs-time-tick midimacs-song-time) 0)
      (midimacs-redraw-top-bar))

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
                        (midimacs-event-do-init event)
                        (eq (midimacs-time-tick midimacs-song-time) 0))

               (setf (midimacs-track-state track)
                     (funcall init
                              channel
                              midimacs-song-time))

               (setf (midimacs-track-last-init-time track) midimacs-abs-time))

             (when run
               (setf (midimacs-track-state track)
                     (funcall run
                              channel
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
  (setq midimacs-state 'stopped)
  (midimacs-redraw-top-bar))

(defun midimacs-code-get-open-buffers ()
  (remove nil (loop for name being the hash-keys of midimacs-codes
                    collect (midimacs-buffer-code name))))

(defun midimacs-code-close-all-buffers ()
  ; TODO
  )

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

(defmacro midimacs-score (notes &rest channel)
  "channel defaults to track channel"
  (cons
   'progn
   (loop for (onset-sym pitch-sym dur-sym) in notes
         with onset = (midimacs-parse-time (midimacs-sym-or-num-to-string onset-sym))
         with pitch = (midimacs-parse-pitch (symbol-name pitch-sym))
         with dur = (midimacs-parse-time (midimacs-sym-or-num-to-string dur-sym))
         collect `(when (midimacs-time= rel-time ,onset)
                    (midimacs-note (or ,channel channel) ,pitch ,dur)))))

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
                  do (forward-line)
                  do (move-to-column col)
                  collect col)))))

(defun midimacs-read-char-no-quit (q)
  (let* ((inhibit-quit t)
         (res (read-char q)))
    (if (= res 7) ;; C-g
        nil
      res)))

(defun midimacs-x-jump ()
  (interactive)
  (let* ((background-overlay (midimacs-x-jump-background-overlay))
         (overlay-map (midimacs-x-jump-make-overlay-map))
         (jump-char (midimacs-read-char-no-quit "Jump to character: ")))
    (when jump-char
      (let* ((c-p-overlay (assoc jump-char overlay-map))
             (jump-point (car (cdr c-p-overlay))))
        (if jump-point
            (goto-char jump-point)
          (message "Invalid jump point"))))

    (delete-overlay background-overlay)

    (loop for (c . (p . overlay)) in overlay-map
          do (delete-overlay overlay))))

(defun midimacs-x-jump-background-overlay ()
  (let ((overlay (make-overlay (window-start) (window-end))))
    (overlay-put overlay 'face 'midimacs-x-jump-face-background)
    overlay))

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
    (overlay-put ol 'face 'midimacs-x-jump-face-foreground)
    (overlay-put ol 'display c)
    ol))

(defun midimacs-set-tempo (bpm)
  (interactive "nBeats per minute: ")
  (let ((state midimacs-state))
    (midimacs-stop)
    (setq midimacs-bpm bpm)
    (when (eq state 'playing)
      (midimacs-play))))


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

(provide 'midimacs)
;;; midimacs.el ends here
