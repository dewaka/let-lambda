(defvar gx 1)

(defun print-gx ()
  (print gx))

(defun bar ()
  (print-gx))

(defun foo ()
  (progn (bar)
         (setf gx 3)
         (bar)))

;; Closures
(defun get-counter ()
  (let ((x 0))
    (lambda () (incf x))))

;; Use counter
(defun use-counter ()
  (let ((counter (get-counter)))
    (loop for i from 1 to 10
       do (print (funcall counter)))))
