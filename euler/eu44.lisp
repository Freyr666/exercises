(defun pentagon (x)
	     (/ (* x
		   (- (* 3 x)
		      1))
		2))

(defun pent-p (x)
  (eql (mod (sqrt (+ (* 24 x)
		     1))
	    6)
       5.0))


(defun find-answer (num len)
  (if (eql num len)
      num
      (if (not (eql (mod num
			 6)
		    4.0))
	  (find-answer (+ num 1) len)
	  (let* ((this (pentagon num))
		 (n-th (pentagon (round (+ (/ (* num num) 2)
					   (/ (- 4 num) 6)))))
		 (n-1-th (- n-th this)))
	    (cond ((pent-p (+ n-th this)) num)
		  ((pent-p (+ n-th n-1-th)) num)
		  (t (find-answer (+ num 1) len)))))))

