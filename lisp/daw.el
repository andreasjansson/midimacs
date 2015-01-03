;; TODO: make all globals buffer-local to the seq buffer, include
;; some name in the buffer names

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
  (define-key daw-seq-mode-map (kbd "RET") 'daw-seq-enter)
  (define-key daw-seq-mode-map (kbd "C-x SPC") 'daw-toggle-play)
  (define-key daw-seq-mode-map (kbd "SPC") 'daw-toggle-play)
  (define-key daw-seq-mode-map (kbd "M-SPC") 'daw-play-here)
  (define-key daw-seq-mode-map (kbd "C-<return>") 'daw-position-here)
  (define-key daw-seq-mode-map (kbd "C-x C-s") 'daw-save)
  (define-key daw-seq-mode-map (kbd "C-x C-w") 'daw-save-as)
  (define-key daw-seq-mode-map (kbd "C-x C-f") 'daw-open)
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

(defvar daw-tracks '())
(defvar daw-codes nil)
(defvar daw-repeat-start nil)
(defvar daw-repeat-end daw-length)
(defvar daw-repeat-end nil)
(defvar daw-abs-time nil)
(defvar daw-song-time nil)
(defvar daw-state 'stopped)
(defvar daw-last-tick-time nil)
(defvar daw-filename nil)
(defvar daw-amidicat-proc nil)
(defvar daw-scheduled-note-offs nil)

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
  (last-init-time (daw-make-time)))

(defstruct daw-midi-message
  status
  data1
  data2)

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
  (setq daw-last-tick-time nil)
  (setq daw-filename nil)
  (setq daw-scheduled-note-offs (daw-make-scheduled-note-offs-heap))

  (setq daw-repeat-start (make-daw-time))
  (setq daw-repeat-end (make-daw-time :beat daw-length))

  (daw-code-close-all-buffers)
  (daw-amidicat-proc-init)
  (daw-draw))

(defun daw-amidicat-proc-init ()
  (interactive)
  (let ((buffer-name (daw-amidicat-buffer-name))
        (process-name "daw-amidicat"))
    (when (get-buffer buffer-name)
      (daw-midi-flush-note-offs)
      (when (get-process process-name)
        (delete-process process-name))
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

(defun daw-draw-tracks ()
  (dolist (track daw-tracks)
    (daw-draw-track track)))

(defun daw-draw-track (track)
  (insert (daw-track-name track))
  (insert " ")
  (daw-draw-track-events track)
  (insert "\n"))

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

(defun daw-draw-track-events (track)
  (let ((track-events (daw-track-events track)))
    (dotimes (i daw-length)
      (let ((track-event (elt track-events i)))
        (daw-draw-track-event track-event)))))

(defun daw-draw-track-event (track-event)
  (insert
   (if (eq track-event nil)
       "-"
     (daw-event-string track-event))))

(defun daw-seq-enter ()
  (interactive)
  (let* ((track (daw-current-track))
         (beat (daw-current-beat))
         (time (make-daw-time :beat beat))
         (code (daw-track-code-at track time)))
    (when (and track beat code)
      (daw-code-open-window code))))

(defun daw-set-repeat-start ()
  (interactive)
  (save-excursion
    (forward-char)
    (let ((new-repeat-start (make-daw-time :beat (daw-current-beat))))
      (daw-check-acceptable-repeat new-repeat-start daw-repeat-end)
      (setq daw-repeat-start new-repeat-start)))
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

(defun daw-track-event-at (track time)
  (elt (daw-track-events track) (daw-time-beat time)))

(defun daw-track-code-at (track time)
  (let ((event (daw-track-event-at track time)))
    (when event
      (daw-event-code event))))

(defun daw-track-rel-time (track)
  (daw-time- daw-abs-time (daw-track-last-init-time track)))

(defun daw-check-track (track)
  (unless track
    (user-error "No track here")))

(defun daw-check-beat (beat)
  (unless beat
    (user-error "No beat here")))

(defun daw-add-code (c)
  (interactive "cCode name: ")
  (daw-add-code-init c t))

(defun daw-add-code-init (c do-init)
  (let ((track (daw-current-track))
        (beat (daw-current-beat))
        (code-name (daw-valid-letter c)))
    (daw-check-track track)
    (daw-check-beat beat)

    (daw-get-or-make-code code-name)
    (aset (daw-track-events track) beat (make-daw-event :code-name code-name
                                                        :do-init do-init)))

  (daw-draw)
  (goto-char (1+ (point))))

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
      (let ((line (daw-current-line)))
        (when (>= line 2)
          (nth (- line 2) daw-tracks))))))

(defun daw-current-beat ()
  (interactive)
  (with-current-buffer (daw-buffer-seq)
    (save-excursion
      (let ((col (current-column)))
        (if (>= col 2)
            (- col 2)
          nil)))))

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
  (eval-buffer)

  )

(defun daw-code (name init run)
  (let ((code (daw-get-code name)))
    (setf (daw-code-init code) init
          (daw-code-run code) run
          (daw-code-text code) (buffer-string)))
  (message (concat "updated code " name)))

(cl-defun daw-note (channel pitch duration &optional (velocity 100) (off-velocity 0))
                                        ; TODO: validation; allow symbols for pitch and duration
  (daw-midi-schedule-note-off (daw-time+ daw-abs-time duration)
                              (daw-midi-note-off channel pitch off-velocity))
  (daw-midi-execute (daw-midi-note-on channel pitch velocity)))

(defun daw-midi-note-on (channel pitch velocity)
  (make-daw-midi-message :status #x90
                         :data1 pitch
                         :data2 velocity))

(defun daw-midi-note-off (channel pitch velocity)
  (make-daw-midi-message :status #x80
                         :data1 pitch
                         :data2 velocity))

(defun daw-midi-schedule-note-off (abs-time message)
  (heap-add daw-scheduled-note-offs (list abs-time message)))

(defun daw-midi-execute (message)
  (process-send-string daw-amidicat-proc (daw-midi-serialize message)))

(defun daw-midi-serialize (message)
  (format "%X %X %X\n"
          (daw-midi-message-status message)
          (daw-midi-message-data1 message)
          (daw-midi-message-data2 message)))

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
  (setq daw-last-tick-time nil)
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

(defun daw-track-events-at-time (time)
  (let ((event))
    (loop for track in daw-tracks
          if (setq event (daw-track-event-at track time))
          collect (list track event))))

(defun daw-wait-time ()
  (let* ((target-wait-time (/ 60.0 daw-bpm daw-ticks-per-beat))
         (now (float-time))
         (drift (if daw-last-tick-time
                    (- now daw-last-tick-time target-wait-time)
                  0)))
    (setq daw-last-tick-time now)
    (- target-wait-time drift)))

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

(provide 'daw)
;;; daw.el ends here

