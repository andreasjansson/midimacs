(eval-when-compile
  (require 'cl))
(require 'midimacs-globals)

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
                         "amidicat" "--hex" "--port" "24:0" "--noread"))
    (set-process-filter midimacs-amidicat-proc 'midimacs-amidicat-read)))

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

(defun midimacs-set-velocity (channel velocity)
  (puthash channel (min 127 velocity) midimacs-channel-default-velocities))

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

(cl-defun midimacs-play-note (channel pitch-raw duration-raw &optional (velocity nil) (off-velocity 0))
  (when (not velocity)
    (setq velocity (gethash channel midimacs-channel-default-velocities 100)))

  (let ((pitch (cond ((symbolp pitch-raw) (midimacs-parse-pitch (symbol-name pitch-raw)))
                     ((stringp pitch-raw) (midimacs-parse-pitch pitch-raw))
                     (t pitch-raw)))
        (duration (cond ((symbolp duration-raw) (midimacs-parse-time (symbol-name duration-raw)))
                        ((stringp duration-raw) (midimacs-parse-time duration-raw))
                        ((numberp duration-raw)
                         (make-midimacs-time :beat (floor (/ duration-raw midimacs-ticks-per-beat))
                                             :tick (mod duration-raw midimacs-ticks-per-beat)))
                        (t duration-raw))))

    (midimacs-midi-schedule-note-off (midimacs-time+ midimacs-abs-time duration)
                                     (midimacs-midi-message-note-off channel pitch off-velocity))
    (midimacs-midi-execute (midimacs-midi-message-note-on channel pitch velocity))))

(cl-defun midimacs-play-notes (channel pitches-raw duration-raw &optional (velocity nil) (off-velocity 0))
  (loop for pitch-raw in pitches-raw
        do (midimacs-play-note channel pitch-raw duration-raw velocity off-velocity)))

(provide 'midimacs-midi)
