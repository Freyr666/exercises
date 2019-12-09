(defconstant +input+ "./input")

(defclass canvas ()
  ((planes :type (simple-vector *))
   (x-dim  :type fixnum) 
   (y-dim  :type fixnum)))

(defmethod initialize-instance :after ((o canvas) &key x y)
  (setf (slot-value o 'x-dim) x)
  (setf (slot-value o 'y-dim) y)
  (setf (slot-value o 'planes)
        (make-array (list 1) :element-type `(simple-array fixnum (,x ,y))))
  (setf (aref (slot-value o 'planes) 0)
        (make-array (list x y) :element-type `fixnum)))

(defmethod draw ((o canvas) (layer integer) (x integer) (y integer) (v fixnum))
  (when (< (1- (length (slot-value o 'planes)))
           layer)
    (let ((new-layers (make-array (list (1+ (- layer
                                               (length (slot-value o 'planes)))))
                                  :initial-element nil)))
      (setf (slot-value o 'planes)
            (concatenate 'vector
                         (slot-value o 'planes)
                         (map 'vector
                              (lambda (ignore) (make-array (list (slot-value o 'x-dim)
                                                                 (slot-value o 'y-dim))
                                                           :element-type `fixnum))
                              new-layers)))))
  (setf (aref (aref (slot-value o 'planes) layer) x y) v))

(defmethod look ((o canvas) (layer integer) (x integer) (y integer))
  (aref (aref (slot-value o 'planes) layer) x y))

(defmethod iter ((o canvas) f)
  (let ((layers (length (slot-value o 'planes)))
        (x-dim  (slot-value o 'x-dim))
        (y-dim  (slot-value o 'y-dim)))
    (dotimes (l layers)
      (dotimes (x x-dim)
        (dotimes (y y-dim)
          (funcall f l x y (aref (aref (slot-value o 'planes) l) x y)))))))

(defmethod iter-layer ((o canvas) (layer integer) f)
  (let ((x-dim  (slot-value o 'x-dim))
        (y-dim  (slot-value o 'y-dim)))
    (dotimes (x x-dim)
      (dotimes (y y-dim)
        (funcall f x y (aref (aref (slot-value o 'planes) layer) x y))))))

(defun read-image (stream x y)
  (let ((acc '()))
    (labels ((parse (char)
               (if (or (char< char #\0)
                       (char> char #\9))
                   nil
                   (- (char-int char)
                      (char-int #\0))))
             (decode (lst)
               (let ((canv (make-instance 'canvas :x x :y y)))
                 (loop
                    for v in lst
                    for ind from 0
                    do (multiple-value-bind (lay pos)
                           (truncate ind (* x y))
                         (multiple-value-bind (y-pos x-pos)
                             (truncate pos x)
                           (draw canv lay x-pos y-pos v))))
                 canv)))
      (loop
         for char = (read-char stream nil nil)
         until (null char)
         do (let ((num (parse char)))
              (when num
                (setf acc (cons num acc)))))
      (decode (reverse acc)))))

(defvar *image*
  (with-open-file (stream +input+)
    (read-image stream 25 6)))

;; Part One

(defun compute-fewest-zeroes (image)
  (let ((tbl (make-hash-table)))
    (iter image (lambda (l x y v)
                  (when (zerop v)
                    (when (not (gethash l tbl))
                      (setf (gethash l tbl) 0))
                    (incf (gethash l tbl)))))
    (let ((min-amount '(0 . 1000000000000)))
      (maphash (lambda (k v) (when (< v (cdr min-amount))
                               (setf min-amount (cons k v))))
               tbl)
      (let ((ones 0)
            (twos 0))
        (iter-layer image
                    (car min-amount)
                    (lambda (x y v) (case v
                                      ((1) (incf ones))
                                      ((2) (incf twos)))))
        (* ones twos)))))

;; Part Two

(defun compute-and-render-image (image)
  (let* ((x-dim  (slot-value image 'x-dim))
         (y-dim  (slot-value image 'y-dim))
         (plane  (make-array (list x-dim y-dim)
                             :element-type 'fixpoint
                             :initial-element 2)))
    (iter image
          (lambda (l x y v)
            (let ((cur (aref plane x y)))
              (when (= cur 2)
                (setf (aref plane x y) v)))))
    (loop
       for y from 0 to (1- y-dim)
       do (progn
            (format t "~%")
            (loop
               for x from 0 to (1- x-dim)
               do (case (aref plane x y)
                    ((0) (format t "  "))
                    ((1 2) (format t " #")))))))) 
