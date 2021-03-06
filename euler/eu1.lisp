(defconstant +ROW3+ '(1 . 333))
(defconstant +ROW5+ '(1 . 199))
(defconstant +INTERSECT+ '(1 . 66))

(defun row-sum (row)
  (* (/ (+ (car row) (cdr row))
	2)
     (cdr row)))

(defun answer ()
  (let ((s3 (row-sum +raw3+))
	(s5 (row-sum +raw5+))
	(int (row-sum +intersect+)))
    (- (+ (* s3 3)
	  (* s5 5))
       (* int 15))))
