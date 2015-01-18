(eval-when-compile
  (require 'cl))
(require 'midimacs-globals)

(defun midimacs-parse-pitch (ss)
  (let ((s (cond ((symbolp ss) (symbol-name ss))
                 (t ss))))
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

      (if (equal s "-")
          nil
        (error (format "Couldn't parse pitch \"%s\"" s))))))

(defun midimacs-anything-to-pitch (pitch-raw)
  (cond ((symbolp pitch-raw) (midimacs-parse-pitch (symbol-name pitch-raw)))
        ((stringp pitch-raw) (midimacs-parse-pitch pitch-raw))
        (t pitch-raw)))

(defun midimacs-pitch-to-string (pitch)
  (if pitch
      (let ((octave (- (floor (/ pitch 12)) 1))
            (name (cdr (assoc (% pitch 12) midimacs-pitch-names))))
        (format "%s%d" name octave))
    "-"))

(provide 'midimacs-pitch)
