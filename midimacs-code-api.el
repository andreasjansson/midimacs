(eval-when-compile
  (require 'cl))
(require 'midimacs-globals)

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
           for pitches = (midimacs-extract-pitches pitch-sym)
           for dur = (midimacs-parse-time dur-sym)
           do (setq cum-time (midimacs-time+ cum-time dur))
           when pitches
           collect (append (list 'when `(midimacs-time= rel-time ,onset))
                           (loop for pitch in pitches
                                 collect `(midimacs-play-note (or ,channel channel) ,pitch ,dur)))))))

(defun midimacs-extract-pitches (pitch-sym)
  (cond ((and (listp pitch-sym) (eq (car pitch-sym) '\,))
         (list (eval (car (cdr pitch-sym)))))
        ((listp pitch-sym)
         (loop for ps in pitch-sym
               collect (midimacs-parse-pitch ps)))
        (t (list (midimacs-parse-pitch pitch-sym)))))

(defmacro midimacs-timed (timed-funcs)
  (cons
   'progn
   (loop for (onset-sym on-func dur-sym off-func) in timed-funcs
         for on-time = (midimacs-parse-time onset-sym)
         for dur = (when dur-sym (midimacs-parse-time dur-sym))
         for off-time = (when dur-sym (midimacs-time+ on-time dur))
         collect `(cond ((midimacs-time= rel-time ,on-time)
                         (lambda () ,on-func))
                        ((and ,off-time ,off-func (midimacs-time= rel-time ,off-time))
                         (lambda () ,off-func))))))

(defmacro midimacs-timed-state (timed-funcs)
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

(defmacro midimacs-start (&rest body)
  (setq midimacs-start-func `(lambda () ,@body)))

(defmacro midimacs-every (time-sym &rest body)
  (let ((time (midimacs-parse-time time-sym)))
    `(when (midimacs-time= (midimacs-time% rel-time ,time) (make-midimacs-time)) ,@body)))

(defun midimacs-code (name init run)
  (let ((code (midimacs-get-code name)))
    (setf (midimacs-code-init code) init
          (midimacs-code-run code) run
          (midimacs-code-text code) (buffer-substring-no-properties (point-min) (point-max))))
  (message (concat "updated code " (string name))))

(provide 'midimacs-code-api)
