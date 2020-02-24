(defun longest-subarray-by-sum (sum array)
  "Given an ARRAY of numbers (unsorted)
find the longest subarray whose sum is equal
to SUM. Returns the pair marking the beginning
and the end of an array."
  (labels ((check-sum (sum left right array)
             (cond
               ((zerop sum)
                (list (- right left) left right))
               ((or (< sum 0)
                    (>= left right))
                (list 0))
               (t
                (let ((l (check-sum (- sum (aref array left))
                                    (1+ left)
                                    right
                                    array))
                      (r (check-sum (- sum (aref array right))
                                    left
                                    (1- right)
                                    array)))
                  (if (> (car l) (car r)) l r))))))
    (let ((array-sum (reduce #'+ array)))
      (assert (> array-sum sum))
      (let ((res (check-sum (- array-sum sum)
                            0
                            (1- (array-dimension array 0))
                            array)))
        (if (zerop (car res))
            (error "No such subarray exists")
            (list (second res) (third res)))))))

(defun first-non-rep-char (string)
  "Given a STRING of (lower) characters
finds the first non-repeating one.
E.g. given 'aaabaaacacd' returns b."
  (let ((table (make-hash-table :size 30))
        (stack '()))
    (labels ((traverse (string pos)
               (unless (< pos 0)
                 (let ((cur (aref string pos)))
                   (if (gethash cur table)
                       (progn
                         (incf (gethash cur table))
                         (traverse string (1- pos)))
                       (progn
                         (setf (gethash cur table) 0)
                         (push cur stack)
                         (traverse string (1- pos))))))))
      (traverse string (1- (length string)))
      (dolist (char stack)
        (when (zerop (gethash char table))
          (return char))))))
