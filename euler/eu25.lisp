(defun basic-memo (fun)
  (let ((memory (make-hash-table)))
    #'(lambda (arg)
	(multiple-value-bind (value foundp)
	    (gethash arg memory)
	  (if
	   foundp
	   value
	   (setf (gethash arg memory) (funcall fun arg))))) ))

(defun basic-memoize (fun)
  (setf (symbol-function fun)
	(basic-memo (symbol-function fun))))

(defun fib (n)
  (if (<= n 1)
      N
      (+ (fib (- n 1))
	 (fib (- n 2)))))

(basic-memoize 'fib)

(defun test (n)
  (loop for i from 1 to n do
       (format t "~A : ~A~%" i (fib i))))

(defun num-length (num acc)
  (if (eql num 0)
      acc
      (num-length (floor num 10) (+ acc 1))))
(+ 1
   (* 251
      (+ 5 5 5 4)))

(fib 4752)
