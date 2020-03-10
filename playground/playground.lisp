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

(defun array-multiply-all-but-i (array)
  "Given an ARRAY, returns an OUTPUT where
each element OUTPUT[i] is a multiplication
of \prod(ARRAY[x]) where x != i"
  (let ((prod (reduce #'* array)))
    (map 'vector (lambda (x) (/ prod x)) array)))

(defun test-array-multiply-all-but-i ()
  (let ((cases  '((#(1 2 3 4) . #(24 12 8 6))
                  (#(6 12 8 3) . #(288 144 216 576)))))
    (dolist (c cases)
      (assert (equalp (array-multiply-all-but-i (car c))
                      (cdr c))))))

(defun max-consecutive-subarray-sum (array)
  "Given an ARRAY of integers finds the largest
sum of a contiguous subarray"
  (declare (type (simple-array integer (*)) array)
           (optimize (speed 3)))
  (let* ((max (aref array 0))
         (cur max))
    (do ((i 1))
        ((= i (array-dimension array 0)))
      (setf cur (max (aref array i)
                     (+ (aref array i) cur)))
      (setf max (max cur max))
      (incf i))
    max))

(defun test-max-consecutive-subarray-sum ()
  (let ((cases   '((#(-2 2 5 -11 6) . 7))))
    (dolist (c cases)
      (assert (equalp (max-consecutive-subarray-sum (car c))
                      (cdr c))))))
