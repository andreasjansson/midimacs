(eval-when-compile
  (require 'cl))
(require 'midimacs-globals)

(defun midimacs-amidicat-init ()
  (interactive)
  (loop for direction in '(input output)
        when (midimacs-amidicat-proc-is-active direction)
          do (midimacs-amidicat-proc-kill direction)
        when (midimacs-amidicat-buffer-has-process direction)
          do (midimacs-amidicat-proc-kill direction)
        when (midimacs-amidicat-buffer-is-open direction)
          do (midimacs-amidicat-buffer-close direction))

  (midimacs-amidicat-proc-open 'input)
  (midimacs-amidicat-proc-open 'output "--noread")

  (when (midimacs-amidicat-proc 'input)
   (set-process-filter (midimacs-amidicat-proc 'input) 'midimacs-amidicat-read)))

(defun* midimacs-amidicat-proc-open (direction &optional (noread nil))
  (let* ((port (midimacs-midi-port direction))
         (direction-name (symbol-name direction))
         (process-args (list (midimacs-amidicat-proc-name direction)
                             (midimacs-amidicat-buffer-name direction)
                             "amidicat" "--hex" "--port" port)))
    (when noread
      (setq process-args (nconc process-args (list "--noread"))))
    (when (not (equal port "DEBUG"))
      (apply 'start-process process-args)
      (sit-for 0.2)                     ; wait for proc to start
      (unless (midimacs-amidicat-proc-is-active direction)
        (display-warning 'midimacs (concat "Failed to open midimacs " direction-name " port " port
                                           ", try `M-x customize-variable midimacs-midi-"
                                           direction-name "-port` and `M-! amidicat --list`"))
        (midimacs-amidicat-buffer-close direction)))))

(defun midimacs-midi-port (direction)
  (cond ((eq direction 'input) midimacs-midi-input-port)
        ((eq direction 'output) midimacs-midi-output-port)))

(defun midimacs-amidicat-proc-is-active (direction)
  (let ((proc (midimacs-amidicat-proc direction)))
    (when proc
      (member (process-status proc) '(run signal)))))

(defun midimacs-amidicat-buffer-has-process (direction)
  (get-buffer-process (midimacs-amidicat-buffer direction)))

(defun midimacs-amidicat-proc-kill (direction)
  (delete-process (midimacs-amidicat-proc direction)))

(defun midimacs-amidicat-buffer-is-open (direction)
  (midimacs-amidicat-buffer direction))

(defun midimacs-amidicat-buffer (direction)
  (get-buffer (midimacs-amidicat-buffer-name direction)))

(defun midimacs-amidicat-buffer-close (direction)
  (kill-buffer (midimacs-amidicat-buffer direction)))

(defun midimacs-amidicat-proc (direction)
  (get-process (midimacs-amidicat-proc-name direction)))

(defun midimacs-amidicat-proc-name (direction)
  ;; process same name as buffer
  (midimacs-amidicat-buffer-name direction))

(defun midimacs-amidicat-buffer-name (direction)
  (concat "*midimacs-amidicat-" (symbol-name direction) "-proc*"))

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

(defun midimacs-midi-message-all-notes-off (channel)
  (make-midimacs-midi-message :status (+ #xB0 channel)
                              :data1 #x7B
                              :data2 0))

(defun midimacs-midi-message-is-note-on (message)
  (eq (elt (format "%02X" (midimacs-midi-message-status message)) 0) ?9))

(defun midimacs-midi-message-is-note-off (message)
  (let ((status-byte-char-0 (elt (format "%02X" (midimacs-midi-message-status message)) 0)))
    (or (eq status-byte-char-0 ?8)
        (and (eq status-byte-char-0 ?9)
             (eq (midimacs-midi-message-data2 message) 0)))))

(defun midimacs-program-change (channel program)
  (midimacs-midi-execute (midimacs-midi-message-program-change channel program)))

(defun midimacs-set-velocity (channel velocity)
  (puthash channel (min 127 velocity) midimacs-channel-default-velocities))

(defun midimacs-midi-schedule-note-off (abs-time channel pitch off-velocity)
  (heap-add midimacs-scheduled-note-offs (list abs-time channel pitch off-velocity)))

(defun midimacs-midi-execute (message)
  (let ((serialized-message (midimacs-midi-serialize message)))
    (cond ((midimacs-amidicat-proc-is-active 'output)
           (process-send-string (midimacs-amidicat-proc 'output) serialized-message))
          ((equal midimacs-midi-output-port "DEBUG")
           (message (string-trim serialized-message))))))

(defun midimacs-midi-serialize (message)
  (let* ((status (midimacs-midi-message-status message))
         (data1 (midimacs-midi-message-data1 message))
         (data2 (midimacs-midi-message-data2 message))
         (status-string (format "%02X" status))
         (data1-string (if data1 (format " %02X" data1) ""))
         (data2-string (if (and data1 data2) (format " %02X" data2) "")))
    (concat status-string data1-string data2-string "\n")))

(defun midimacs-midi-unserialize (s)
  (unless (string-prefix-p "Unable to connect to ALSA port" s)
    (destructuring-bind (status-s data1-s &optional data2-s)
        (split-string s " ")
      (make-midimacs-midi-message :status (string-to-number status-s 16)
                                  :data1 (string-to-number data1-s 16)
                                  :data2 (when data2-s (string-to-number data2-s 16))))))

(defun midimacs-delete-message-heap-until (heap pos)
  (let ((el))
    (loop while (and (setq el (heap-root heap))
                     (midimacs-time<= (nth 0 el) pos))
          collect (heap-delete-root heap))))

(defun midimacs-trigger-note-offs ()
  (let ((note-offs (midimacs-delete-message-heap-until midimacs-scheduled-note-offs midimacs-abs-time)))
    (loop for (time channel pitch off-velocity) in note-offs
          do (midimacs-note-off channel pitch off-velocity))))

(defun midimacs-midi-flush-note-offs ()
  (let ((el))
    (loop while (setq el (heap-delete-root midimacs-scheduled-note-offs))
          do (midimacs-note-off (nth 1 el) (nth 2 el) (nth 3 el)))))

(defun midimacs-amidicat-read (proc string)
  (loop for s in (split-string string "\n")
        when (> (length s) 0)
        do (midimacs-handle-midi-input
            (midimacs-midi-message-set-channel
             (midimacs-midi-unserialize s) midimacs-midi-input-channel))))

(defun midimacs-midi-message-set-channel (message channel)
  (let ((status (midimacs-midi-message-status message)))
   (setf (midimacs-midi-message-status message) (+ status channel))
   message))

(defun midimacs-handle-midi-input (message)
  (midimacs-midi-execute message)
  (when (eq midimacs-state 'recording)
    (cond ((midimacs-midi-message-is-note-off message)
           (midimacs-record-midi-note-off message))
          ((midimacs-midi-message-is-note-on message)
           (midimacs-record-midi-note-on message)))))

(defun midimacs-record-midi-note-on (message)
  (let* ((song-time (midimacs-quantized-song-time))
         (pitch (midimacs-midi-message-data1 message))
         (pitch-string (midimacs-pitch-to-string pitch))
         (initial-duration (make-midimacs-time :beat 999))
         (start-time (midimacs-score-start-time midimacs-recording-score))
         (rel-time (midimacs-time- song-time start-time)))

    (when (midimacs-time> rel-time (make-midimacs-time))
      (midimacs-add-note-to-score midimacs-recording-score rel-time pitch-string initial-duration))))

(defun midimacs-record-midi-note-off (message)
  (let* ((song-time (midimacs-quantized-song-time))
         (pitch (midimacs-midi-message-data1 message))
         (pitch-string (midimacs-pitch-to-string pitch))
         (start-time (midimacs-score-start-time midimacs-recording-score))
         (rel-time (midimacs-time- song-time start-time)))

    (setf (midimacs-score-notes midimacs-recording-score)
          (loop for (tm p d) in (midimacs-score-notes midimacs-recording-score)
                if (and (equal p pitch-string) (midimacs-time= d (make-midimacs-time :beat 999)))
                     collect (let* ((repeat-time (midimacs-time- midimacs-repeat-end midimacs-repeat-start))
                                    (raw-time (midimacs-time- rel-time tm))
                                    (duration (midimacs-time-mod raw-time repeat-time)))
                               (list tm p duration))
                else
                     collect (list tm p d)))))

(defun* midimacs-play-note (channel pitch-raw duration-raw &optional (velocity nil) (off-velocity 0))
  (when (not velocity)
    (setq velocity (gethash channel midimacs-channel-default-velocities 100)))

  (let ((pitch (midimacs-anything-to-pitch pitch-raw))
        (duration (midimacs-anything-to-time duration-raw)))

    (midimacs-midi-schedule-note-off (midimacs-time+ midimacs-abs-time duration) channel pitch off-velocity)
    (midimacs-note-on channel pitch velocity)))

(defun* midimacs-play-notes (channel pitches-raw duration-raw &optional (velocity nil) (off-velocity 0))
  (loop for pitch-raw in pitches-raw
        do (midimacs-play-note channel pitch-raw duration-raw velocity off-velocity)))

(defun midimacs-make-scheduled-note-offs-heap ()
  (make-heap (lambda (a b) (midimacs-time< (nth 0 a) (nth 0 b)))))

(defun midimacs-started (channel)
  (gethash channel midimacs-channel-started-notes))

(defun midimacs-sustained (channel)
  (gethash channel midimacs-channel-sustained-notes))

(defun midimacs-stopped (channel)
  (gethash channel midimacs-channel-stopped-notes))

(defun* midimacs-note-on (channel pitch &optional (velocity nil))
  (when (not velocity)
    (setq velocity (gethash channel midimacs-channel-default-velocities 100)))

  (let ((started-notes (midimacs-started channel))
        (sustained-notes (midimacs-sustained channel)))
    (add-to-list 'started-notes pitch)
    (puthash channel started-notes midimacs-channel-started-notes)
    (add-to-list 'sustained-notes pitch)
    (puthash channel sustained-notes midimacs-channel-sustained-notes))

  (when (midimacs-valid-channel channel)
    (midimacs-midi-execute (midimacs-midi-message-note-on channel pitch velocity))))

(defun* midimacs-note-off (channel pitch &optional (off-velocity 0))
  (let ((sustained-notes (midimacs-sustained channel))
        (stopped-notes (midimacs-stopped channel)))
    (puthash channel (delq pitch sustained-notes) midimacs-channel-sustained-notes)
    (add-to-list 'stopped-notes pitch)
    (puthash channel stopped-notes midimacs-channel-stopped-notes))

  (when (midimacs-valid-channel channel)
    (midimacs-midi-execute (midimacs-midi-message-note-off channel pitch off-velocity))))

(defun midimacs-valid-channel (channel)
  (and (>= channel 0)
       (<= channel 15)))

(defun midimacs-reset-channel-started-stopped-notes ()
  (setq midimacs-channel-started-notes (make-hash-table))
  (setq midimacs-channel-stopped-notes (make-hash-table)))

(defun midimacs-record-midi ()
  (interactive)
  (if (eq midimacs-state 'stopped)
      (progn
        (midimacs-record))
    (user-error "Can only start recording from stopped")))

(defun midimacs-all-notes-off (&optional channel)
  (interactive)
  (if channel
      (midimacs-midi-execute (midimacs-midi-message-all-notes-off channel))
    (loop for channel below 16
          do (midimacs-all-notes-off channel))))

(provide 'midimacs-midi)
