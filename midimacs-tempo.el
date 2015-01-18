(eval-when-compile
  (require 'cl))
(require 'midimacs-globals)

(defun midimacs-read-tempo-taps ()
  (loop for prev-time = (float-time)
        for char = (read-char (if tempo
                                  (format "Current tempo: %.2f" tempo)
                                "Tap any key other than SPACE to start tapping, or SPACE to exit"))
        for tempo = (midimacs-calc-tap-tempo times)
        until (= char ? )
        collect (- (float-time) prev-time) into times
        finally (return tempo)))

(defun midimacs-calc-tap-tempo (times)
  (let ((good-times (cdr times)))
    (when good-times
      (/ 60 (/ (apply '+ good-times) (length good-times))))))



(provide 'midimacs-tempo)
