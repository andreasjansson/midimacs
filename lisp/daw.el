;; TODO: make all globals buffer-local to the seq buffer, include
;; some name in the buffer names

(eval-when-compile
  (require 'cl))

(define-derived-mode daw-seq-mode special-mode "daw-seq-mode"
  (daw-seq-define-letter-keys)
  (define-key daw-seq-mode-map (kbd "=") 'daw-seq-add-track)
  (define-key daw-seq-mode-map (kbd "RET") 'daw-seq-enter)
  (define-key daw-seq-mode-map (kbd "C-x SPC") 'daw-seq-toggle-play)
  (define-key daw-seq-mode-map (kbd "SPC") 'daw-seq-toggle-play)
  (define-key daw-seq-mode-map (kbd "C-x C-s") 'daw-save)
  (define-key daw-seq-mode-map (kbd "C-x C-w") 'daw-save-as)
  (define-key daw-seq-mode-map (kbd "C-x C-f") 'daw-open)
  (setq truncate-lines t))

(define-derived-mode daw-code-mode emacs-lisp-mode "daw-code-mode"
  (define-key daw-code-mode-map (kbd "C-x SPC") 'daw-seq-toggle-play)
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
(defvar daw-repeat-start 0)
(defvar daw-repeat-end daw-length)
(defvar daw-repeat-end 256)
(defvar daw-position-beat 0)
(defvar daw-position-tick 0)
(defvar daw-state 'paused)
(defvar daw-last-tick-time nil)
(defvar daw-filename nil)

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
  (rel-beat 0))

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
  (setq daw-position-beat 0)
  (setq daw-position-tick 0)
  (setq daw-state 'paused)
  (setq daw-last-tick-time nil)
  (setq daw-filename nil)

  (daw-code-close-all-buffers)
  (daw-draw))

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
         (repeat-start-col (+ daw-repeat-start 1))
         (repeat-end-col (+ daw-repeat-end 2))
         (position-col (+ (floor daw-position-beat) 2))
         (chars `((,repeat-start-col . "▕")
                  (,repeat-end-col . "▏")
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
        ((eq daw-state 'paused) "◾")))

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

(defun daw-enter ()
  (interactive)
  (let ((track (daw-current-track))
        (beat (daw-current-beat)))
    (when (and track beat)
      (daw-code-open-window (daw-track-code-at track beat)))))

(defun daw-add-track ()
  (interactive)
  (let ((track (make-daw-track :name (daw-next-track-name)
                               :events (make-vector daw-length nil)
                               :state nil)))
    (setq daw-tracks (append daw-tracks (list track))))

  (daw-draw))

(defun daw-next-track-name ()
  (string (elt daw-letters (length daw-tracks))))

(defun daw-event-at (track beat)
  (elt (daw-track-events track) beat))

(defun daw-track-code-at (track beat)
  (let ((event (daw-event-at track beat)))
    (when event
      (daw-event-code event))))

(defun daw-add-code (c)
  (interactive "cCode name: ")
  (daw-add-code-init c t))

(defun daw-add-code-init (c do-init)
  (let ((track (daw-current-track))
        (beat (daw-current-beat))
        (code-name (daw-valid-letter c)))
    (unless track
      (user-error "No track there"))
    (unless beat
      (user-error "No beat there"))

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
 (lambda (beat)

   nil)

 ;; run
 (lambda (beat rel-beat tick state)

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

(defun daw-toggle-play ()
  (interactive)
  (cond ((eq daw-state 'playing) (daw-pause))
        ((eq daw-state 'paused) (daw-play)))
  (daw-draw))

(defun daw-play ()
  (setq daw-state 'playing)
  (setq daw-last-tick-time nil)
  (daw-tick))

(defun daw-tick ()
  (daw-trigger-events)
  (daw-update-position)
  (daw-draw)
  (when (eq daw-state 'playing)
    (run-at-time (daw-wait-time) nil 'daw-tick)))

(defun daw-update-position ()
  (if (< daw-position-tick (1- daw-ticks-per-beat))
      (incf daw-position-tick)
    (setq daw-position-tick 0)
    (incf daw-position-beat)

    (dolist (track daw-tracks)
      (incf (daw-track-rel-beat track)))

    (when (eq daw-position-beat daw-repeat-end)
      (setq daw-position-beat daw-repeat-start))))

(defun daw-trigger-events ()
  (let ((events (daw-events-at-time daw-position-beat))
        (beat daw-position-beat)
        (tick daw-position-tick))

    (loop for (track event) in events do
          (let* ((code (daw-event-code event))
                 (init (daw-code-init code))
                 (run (daw-code-run code)))

            (when (and (daw-event-do-init event) init (eq tick 0))
              (setf (daw-track-state track) (funcall init beat))
              (setf (daw-track-rel-beat track) 0))

            (when run
              (setf (daw-track-state track)
                    (funcall run
                             beat
                             (daw-track-rel-beat track)
                             tick
                             (daw-track-state track))))))))

(defun daw-events-at-time (beat)
  (let ((events)
        (event))
    (dolist (track daw-tracks)
      (setq event (daw-event-at track beat))
      (when event
        (setq events (append events (list (list track event))))))
    events))

(defun daw-wait-time ()
  (let* ((target-wait-time (/ 60.0 daw-bpm daw-ticks-per-beat))
         (now (float-time))
         (drift (if daw-last-tick-time
                    (- now daw-last-tick-time target-wait-time)
                  0)))
    (setq daw-last-tick-time now)
    (- target-wait-time drift)))

(defun daw-pause ()
  (setq daw-state 'paused))

(defun daw-code-get-open-buffers ()
  (remove nil (loop for name being the hash-keys of daw-codes
                    collect (daw-buffer-code name))))

(defun daw-code-close-all-buffers ()
  (dolist (letter (string-to-list daw-letters))
    (let ((buffer-name (daw-buffer-code-name (string letter))))
      (when (get-buffer buffer-name)
        (delete-window (get-buffer-window buffer-name))
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
                         daw-repeat-end
                         daw-position-beat)))

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
                       repeat-end
                       position-beat)
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
    (setq daw-position-beat position-beat)))

(provide 'daw)
;;; daw.el ends here

