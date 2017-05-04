(defun generator (&key (start 1) count (step 1))
  (loop repeat count for i from start by step collect i))

(defun len-counter (n acc)
  (if (eql n 1)
      acc
      (if (evenp n)
	  (len-counter (/ n 2) (1+ acc))
	  (len-counter (+ (* n 3) 1) (1+ acc)))))

(defun len-counter-l (n acc lst)
    (if (eql n 1)
      '(acc lst) 
      (if (evenp n)
	  (len-counter-l (/ n 2) (1+ acc) (append lst '(n)))
	  (len-counter-l (+ (* n 3) 1) (1+ acc) (append lst '(n))))))
      
(defun collatz (n)
  (len-counter n 1))

(defun coll-lst (lst)
  (mapcar #'(lambda (x) (list x (collatz x))) lst))

;;(remove-if #'(lambda (x) (<= (cadr x) 507)) (coll-lst (generator :start 0 :count 999999)))
