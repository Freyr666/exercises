(defconstant +numbers+
  '((1 3) (2 3) (3 5) (4 4) (5 4) (6 3) (7 5) (8 5) (9 4)))

(defconstant +unique+
  '((10 3) (11 6) (12 6) (13 8) (14 8) (15 7) (16 7) (17 9) (18 8) (19 8)))

(defconstant +tens+
  '((20 6) (30 6) (40 5) (50 5) (60 5) (70 7) (80 6) (90 6)))

(defconstant +hundred+
  7)

(defun fold-tuple-sum (lst)
  (cadr (reduce #'(lambda (x y) (list (car x)
				      (+ (cadr x)
					 (cadr y)))) lst)))

;; 20-99
;; unique + tens + 8 nums

(defun counter ()
  (+
   ;; first nine numbers for each xx0
   (* 90
      (fold-tuple-sum +numbers+))
   ;; unique numbers
   (* 10
      (fold-tuple-sum +unique+))
   ;; tens
   (* 100
      (fold-tuple-sum +tens+))
   ;; hundreds
   (* 100
      (+ (fold-tuple-sum +numbers+)
	 (* 9 +hundred+)))
   ;; ands
   (* 3
      99
      9)
   ;; thousand
   11))
      
