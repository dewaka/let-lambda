;; Sleep units macro enhances the usability of built in sleep function
;; Provides easy to follow, familiar notation with symbols indicating time
;; markers such as seconds (s), minutes (m), hours (h) etc.
(defmacro sleep-units (value unit)
  `(sleep
    (* ,value
     ,(case unit
            ((s) 1)
            ((m) 60)
            ((h) 3600)
            ((d) 86400)
            ((ms) 1/1000)
            ((us) 1/1000000)))))

;; Example usage:
;; Following function prints 1 to 10 taking half a second for each number
;; to be printed
(defun su-example ()
  (loop for i from 1 to 10
     do (progn (print i)
               (sleep-units .5 s))))


;; Following unit-of-time macros provides a better abstraction than specific
;; use case macro of sleep-units because it is more general
(defmacro unit-of-time (value unit)
  `(* ,value
      ,(case unit
            ((s) 1)
            ((m) 60)
            ((h) 3600)
            ((d) 86400)
            ((ms) 1/1000)
            ((us) 1/1000000))))

;; We can define the sleep-units macro using the more general unit-of-time macro
(defmacro sleep-units2 (value unit)
  `(sleep (unit-of-time ,value ,unit)))
