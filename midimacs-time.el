(eval-when-compile
  (require 'cl))

(defun midimacs-time-to-ticks (time)
  (let ((time (midimacs-anything-to-time time)))
    (+ (midimacs-time-tick time)
       (* (midimacs-time-beat time) midimacs-ticks-per-beat))))

(defun midimacs-ticks-to-time (ticks)
  (let ((beat (floor (/ ticks midimacs-ticks-per-beat)))
        (tick (% ticks midimacs-ticks-per-beat)))
    (make-midimacs-time :beat beat
                        :tick tick)))

(defun midimacs-time< (a b)
  (let* ((a (midimacs-anything-to-time a))
         (b (midimacs-anything-to-time b))
         (beat-a (midimacs-time-beat a))
         (beat-b (midimacs-time-beat b))
         (tick-a (midimacs-time-tick a))
         (tick-b (midimacs-time-tick b)))

    (cond ((< beat-a beat-b) t)
          ((and (= beat-a beat-b) (< tick-a tick-b)) t))))

(defun midimacs-time= (a b)
  (let ((a (midimacs-anything-to-time a))
        (b (midimacs-anything-to-time b)))
    (and (= (midimacs-time-beat a) (midimacs-time-beat b))
         (= (midimacs-time-tick a) (midimacs-time-tick b)))))

(defun midimacs-time<= (a b)
  (or (midimacs-time= a b) (midimacs-time< a b)))

(defun midimacs-time> (a b)
  (not (midimacs-time<= a b)))

(defun midimacs-time>= (a b)
  (not (midimacs-time< a b)))

(defun midimacs-time+ (&rest times)
  (funcall (midimacs-time-op '+) times))

(defun midimacs-time- (&rest times)
  (funcall (midimacs-time-op '-) times))

(defun midimacs-time-min (&rest times)
  (funcall (midimacs-time-op 'min) times))

(defun midimacs-time-max (&rest times)
  (funcall (midimacs-time-op 'max) times))

(defun midimacs-time-op (f)
  (lexical-let ((f f))
    (lambda (times)
      (let* ((times-ticks (mapcar 'midimacs-time-to-ticks times))
             (ticks (apply f times-ticks)))
        (midimacs-ticks-to-time ticks)))))

(defun midimacs-time* (time factor)
  (let ((ticks (midimacs-time-to-ticks time)))
    (midimacs-ticks-to-time (* factor ticks))))

(defun midimacs-time/ (a b)
  (let ((ticks-a (midimacs-time-to-ticks a))
        (ticks-b (midimacs-time-to-ticks b)))
    (/ (float ticks-a) ticks-b)))

(defun midimacs-time% (a b)
  (let ((ticks-a (midimacs-time-to-ticks a))
        (ticks-b (midimacs-time-to-ticks b)))
    (midimacs-ticks-to-time (% ticks-a ticks-b))))

(defun midimacs-time-mod (a b)
  (let ((ticks-a (midimacs-time-to-ticks a))
        (ticks-b (midimacs-time-to-ticks b)))
    (midimacs-ticks-to-time (mod ticks-a ticks-b))))

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

(defun midimacs-parse-time (ss)
  (let ((s (midimacs-sym-or-num-to-string ss)))
    (if (string-match (midimacs-time-regex) s)
        (let* ((beat-s (or (match-string 1 s) (match-string 2 s)))
               (frac-num-s (or (match-string 3 s) (match-string 5 s)))
               (frac-denom-s (or (match-string 4 s) (match-string 6 s)))
               (beat (if beat-s (string-to-number beat-s) 0))
               (frac-num (when frac-num-s (string-to-number frac-num-s)))
               (frac-denom (when frac-denom-s (string-to-number frac-denom-s)))
               (frac-ticks (if frac-num (midimacs-frac-to-tick frac-num frac-denom) 0))
               (beat-ticks (* beat midimacs-ticks-per-beat))
               (ticks (+ frac-ticks beat-ticks)))
          (midimacs-ticks-to-time ticks))
      (error (format "Couldn't parse time: %s" s)))))

(defun midimacs-frac-to-tick (num denom)
  (let ((tick (/ (float (* num midimacs-ticks-per-beat)) denom)))
    (unless (= (truncate tick) tick)
      (error (format "Invalid tick fraction %d/%d at %d ticks per beat (tick %f)"
                     num denom midimacs-ticks-per-beat tick)))
    (truncate tick)))

(defun midimacs-time-to-string (time)
  (let* ((beat (midimacs-time-beat time))
         (tick (midimacs-time-tick time))
         (gcd (gcd tick midimacs-ticks-per-beat))
         (frac-num (/ tick gcd))
         (frac-denom (/ midimacs-ticks-per-beat gcd)))
    (format "%s%s%s"
            (if (and (= beat 0) (not (= frac-num 0)))
                ""
              (format "%d" beat))
            (if (or (= beat 0) (= frac-num 0))
                ""
              "+")
            (if (= frac-num 0)
                ""
              (format "%d/%d" frac-num frac-denom)))))

(defun midimacs-anything-to-time (time-raw)
  (cond ((symbolp time-raw) (midimacs-parse-time (symbol-name time-raw)))
        ((numberp time-raw) (midimacs-parse-time (number-to-string time-raw)))
        ((stringp time-raw) (midimacs-parse-time time-raw))
        ((midimacs-time-p time-raw) time-raw)
        (t (error (format "Couldn't parse time: %s" (prin1-to-string time-raw))))))

(defun midimacs-time-quantize (time subdiv)
  (let* ((time-ticks (midimacs-time-to-ticks time))
         (subdiv-ticks (midimacs-time-to-ticks subdiv))
         (quantized-ticks (* (round (/ time-ticks (float subdiv-ticks))) subdiv-ticks)))
    (midimacs-ticks-to-time quantized-ticks)))

(provide 'midimacs-time)

;; Local variables:
;; byte-compile-warnings: (not cl-functions)
;; End:
