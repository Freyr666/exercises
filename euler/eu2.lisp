
(defun fib-gen (max)
  (let ((start-value '(0 1 1)))
    (loop
       (let* ((len (length start-value))
	      (lst (nth (- len 1) start-value))
	      (llst (nth (- len 2) start-value))
	      (sum (+ lst llst)))
	 (when (>= sum max)
	   (return start-value))
	 (setq start-value (append start-value (list sum)))))))

(defun fib-sum-even (max)
  (let ((lst (fib-gen max)))
    (reduce #'+
	    (remove-if-not #'evenp lst))))
