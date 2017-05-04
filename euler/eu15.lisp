;;
(defun generator (&key (start 1) count (step 1))
  (loop repeat count for i from start by step collect i))
;;
;; ---1--1--1--1--1--1--
;; 1 (2) 3  4  5  6  7
;; 1  3 (6)10 15 21
;; 1  4 10(20)35
;; 1  5 15 35 
;; 1  6 21
;; 1  7
(defun path-counter (x)
  "(x+x)!/(x!)^2"
  (let ((div (reduce #'*
		     (generator :start (+ x 1) :count x)))
	(fac (reduce #'*
		     (generator :start 1 :count x))))
    (/ div
       fac)))

