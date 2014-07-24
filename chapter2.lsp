(defvar gx 1)

(defun print-gx ()
  (print gx))

(defun bar ()
  (print-gx))

(defun foo ()
  (progn (bar)
         (setf gx 3)
         (bar)))
