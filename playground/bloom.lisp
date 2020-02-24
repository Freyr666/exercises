
(deftype bitarray (size)
  `(simple-array bit (,size)))

(defstruct bf
  (len 0 :type integer)
  (array nil :type (bitarray *)))

(defgeneric hash (size value))

(defun make-bloom-filter (&optional (prob 0.01))
  (assert (and (<= prob 1)
               (>= prob 0)))
  (let ((len (ceiling 1 prob)))
    (make-bf :len len
             :array (make-array (list len)
                                :element-type 'bit))))

(defun bloom-filter-insert (bf val)
  (let ((hv (hash (slot-value bf 'len) val))
        (a  (slot-value bf 'array)))
    (dotimes (i (array-dimension a 0))
      (setf (aref a i)
            (aref hv i)))))

(defun bloom-filter-search (bf val)
  (let ((hv  (hash (slot-value bf 'len) val))
        (a   (slot-value bf 'array)))
    (dotimes (i (array-dimension a 0))
      (when (and (not (zerop (aref hv i)))
                 (zerop (aref a i)))
        (return-from bloom-filter-search nil)))
    t))
