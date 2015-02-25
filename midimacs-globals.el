(eval-when-compile
  (require 'cl))

(defcustom midimacs-length 999
  "number of beats"
  :group 'midimacs
  :type 'integer)

(defcustom midimacs-bpm 120
  "beats per minute"
  :group 'midimacs
  :type 'number)

(defcustom midimacs-midi-output-port "DEBUG"
  "Output port for amidicat; run midimacs-amidicat-init after changing"
  :group 'midimacs
  :type 'string)

(defcustom midimacs-midi-input-port "DEBUG"
  "Input port for amidicat; run midimacs-amidicat-init after changing"
  :group 'midimacs
  :type 'string)

(defcustom midimacs-midi-input-channel "DEBUG"
  "MIDI input channel"
  :group 'midimacs
  :type 'integer)

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
                                 (11 . B)))

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
(defvar midimacs-last-tick-seconds nil) ; used for recording
(defvar midimacs-filename nil)
(defvar midimacs-scheduled-note-offs nil)
(defvar midimacs-repeat-start-overlay nil)
(defvar midimacs-repeat-end-overlay nil)
(defvar midimacs-play-overlay nil)
(defvar midimacs-midi-history nil)
(defvar midimacs-recording-score nil)
(defvar midimacs-global-init-func nil)
(defvar midimacs-channel-default-velocities nil)
(defvar midimacs-channel-started-notes nil)
(defvar midimacs-channel-sustained-notes nil)
(defvar midimacs-channel-stopped-notes nil)

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
  end-time
  do-init)

(defstruct midimacs-code
  name
  text
  init
  run)

(defstruct midimacs-track
  channel
  events
  mute
  solo
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

(defconst midimacs-default-recording-duration (make-midimacs-time :tick (/ midimacs-ticks-per-beat 2)))

(provide 'midimacs-globals)
