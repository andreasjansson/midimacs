(eval-when-compile
  (require 'cl))
(require 'midimacs-globals)

(defmacro midimacs-score (&rest notes)
  (let ((cum-time (make-midimacs-time)))
    (cons
     'progn
     (loop for symbols in notes
           for onset-sym = (when (= (length symbols) 3) (nth 0 symbols))
           for pitch-sym = (if (= (length symbols) 3) (nth 1 symbols) (nth 0 symbols))
           for dur-sym = (if (= (length symbols) 3) (nth 2 symbols) (nth 1 symbols))
           for onset = (if onset-sym (midimacs-parse-time onset-sym) cum-time)
           for dur = (midimacs-parse-time dur-sym)
           do (setq cum-time (midimacs-time+ cum-time dur))
           collect (append `(when (midimacs-time= rel-time ,onset)
                              (loop for pitch in (midimacs-extract-pitches (quote ,pitch-sym))
                                    if pitch
                                    collect (midimacs-play-note channel pitch ,dur))))))))

(defun midimacs-extract-pitches (pitch-sym)
  (cond ((and (listp pitch-sym) (eq (car pitch-sym) '\,))
         (let ((evaled (eval (car (cdr pitch-sym)))))
           (if (listp evaled)
               evaled
             (list evaled))))
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

(defmacro midimacs-global-init (&rest body)
  (setq midimacs-global-init-func `(lambda () ,@body)))

(defmacro* midimacs-every (time &rest body)
  (let ((time (midimacs-parse-time time-sym)))
    `(when (midimacs-time= (midimacs-time% rel-time ,time) (make-midimacs-time)) ,@body)))

(defmacro midimacs-init (args &rest body)
  (destructuring-bind (channel song-time length state) args
    (let ((code (midimacs-current-buffer-code)))
      (setf (midimacs-code-init code) `(lambda (,channel ,song-time ,length ,state) ,@body)))))

(defmacro midimacs-run (args &rest body)
  (destructuring-bind (channel song-time rel-time state) args
    (let ((code (midimacs-current-buffer-code)))
      (setf (midimacs-code-run code) `(lambda (,channel ,song-time ,rel-time ,state) ,@body)))))

(provide 'midimacs-code-api)
