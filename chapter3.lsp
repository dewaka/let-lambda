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

;; Named lets as in Scheme
(defmacro nlet (n letargs &rest body)
  `(labels ((,n ,(mapcar #'car letargs)
            ,@body))
     (,n ,@(mapcar #'cadr letargs))))

;; Using nlet
(defun nlet-factorial (n)
  (nlet fact ((n n))
        (if (zerop n)
            1
            (* n (fact (1- n))))))

;; nlet can be better understood by macroexpanding an expression containing an
;; expression containing a use of nlet
;;
;;> (macroexpand `(nlet fact ((n n)) (if (zerop n) 1 (* n (fact (1- n))))))
;;
;; (LABELS ((FACT (N)
;;            (IF (ZEROP N)
;;                1
;;                (* N (FACT (1- N))))))
;;   (FACT N))

(defun fact (n)
  (if (zerop n)
      1
      (* n (fact (1- n)))))

;; Using local functional definitions
(defun welcome ()
  (labels
      ;; local function to read a name
      ((read-name ()
         (progn (format t "Please enter your name: ")
                (read-line)))
      ;; local function to print a greeting message
       (greet (name)
         (format t "Hello there ~a" name)))
    (greet (read-name))))

;; NIF buggy version
;; This macro is buggy because even though the name obscure-name sounds obscure
;; is not a gensym - thus it could be shadowing a legitimate variable referred
;; one of the four provided expressions
;; Thus, this is indeed a buggy macro
(defmacro nif-buggy (expr pos zero neg)
  `(let ((obscure-name ,expr))
     (cond ((plusp obscure-name) ,pos)
           ((zerop obscure-name) ,zero)
           (t ,neg))))

;; The right way to do that is to of course use a gensym to store the expr value
;; To simulate or rather emulate functional call with macros we have to make
;; sure that we only evaluate expr value once and only once


;; Safe nif macro
(defmacro nif (expr pos zero neg)
  (let ((g (gensym)))
    `(let ((,g ,expr))
       (cond ((plusp ,g) ,pos)
             ((zerop ,g) ,zero)
             (t ,neg)))))

;; Naif examples
(defun show-message-on-num (num)
  (nif num
       (print "Number is positive")
       (print "It's Zero")
       (print "Number is negative")))

(defun div-p (n m)
  (eq (mod n m) 0))

(defun fizz-buzz-p (n)
  (cond ((and (div-p n 3) (div-p n 5)) (print "Fizz-Buzz"))
        ((div-p n 3) (print "Fizz"))
        ((div-p n 5) (print "Buzz"))
        (t (print n))))

(defun fizz-buzz (n)
  (dotimes (i n) (fizz-buzz-p i)))

;; G-BANG-SYMBOL-PREDICATE
(defun g!-symbol-p (s)
  (and (symbolp s)
       (> (length (symbol-name s)) 2)
       (string= (symbol-name s)
                "G!"
                :start1 0
                :end1 2)))

(defmacro defmacro/g! (name args &rest body)
  (let ((syms (remove-duplicates
               (remove-if-not #'g!-symbol-p
                              (flatten body)))))
    `(defmacro ,name ,args
       (let ,(mapcar
              (lambda (s)
                `(,s (gensym ,(subseq
                               (symbol-name s)
                               2))))
              syms)
         ,@body))))
