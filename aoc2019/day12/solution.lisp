(deftype vec3 ()
  `(simple-array integer (3)))

(defun make-vec3 (x y z)
  (the vec3 (make-array
             '(3)
             :element-type 'integer
             :initial-contents (list x y z))))

(defmacro vec3-x (v)
  `(aref ,v 0))

(defmacro vec3-y (v)
  `(aref ,v 1))

(defmacro vec3-z (v)
  `(aref ,v 2))

(defstruct (celestial (:copier nil))
  (pos (make-vec3 0 0 0) :type vec3)
  (vel (make-vec3 0 0 0) :type vec3))

(defun copy-celestial (c)
  (make-celestial :pos (copy-seq (celestial-pos c))
                  :vel (copy-seq (celestial-vel c))))

(defun move (body)
  (declare (type celestial body))
  (setf (celestial-pos body)
        (map 'vec3
             #'+
             (celestial-pos body)
             (celestial-vel body))))

(defmethod adjust-velocity ((first celestial) (second celestial))
  (let ((pos-dif (map 'vec3
                      #'-
                      (celestial-pos first)
                      (celestial-pos second))))
    (dotimes (i 3)
      (let ((shift (aref pos-dif i)))
        (cond ((< shift 0)
               (setf (aref (celestial-vel first) i)
                     (1+ (aref (celestial-vel first) i)))
               (setf (aref (celestial-vel second) i)
                     (1- (aref (celestial-vel second) i))))
              ((> shift 0)
               (setf (aref (celestial-vel first) i)
                     (1- (aref (celestial-vel first) i)))
               (setf (aref (celestial-vel second) i)
                     (1+ (aref (celestial-vel second) i)))))))))

(defvar *coords*
  (list (make-celestial :pos (make-vec3 5 13 -3))
        (make-celestial :pos (make-vec3 18 -7 13))
        (make-celestial :pos (make-vec3 16 3 4))
        (make-celestial :pos (make-vec3 0 8 8)))
  "<x=5, y=13, z=-3>
   <x=18, y=-7, z=13>
   <x=16, y=3, z=4>
   <x=0, y=8, z=8>")

(defun energy (bodies)
  (labels ((single (body)
             (* (reduce #'+
                        (map 'list #'abs (celestial-pos body)))
                (reduce #'+
                        (map 'list #'abs (celestial-vel body))))))
    (reduce #'+ (mapcar #'single bodies))))

(defun simulate (steps bodies)
  (labels ((adjust (h tl)
             (mapcar (lambda (o) (adjust-velocity h o)) tl)
             (when tl
               (adjust (car tl) (cdr tl)))))
    (dotimes (i steps)
      (adjust (car bodies) (cdr bodies))
      (mapcar #'move bodies))
    (values bodies (energy bodies))))

;; Part Two

(setq *coords* (list (make-celestial :pos (make-vec3 5 13 -3))
                     (make-celestial :pos (make-vec3 18 -7 13))
                     (make-celestial :pos (make-vec3 16 3 4))
                     (make-celestial :pos (make-vec3 0 8 8))))

(setq *test-coords* (list (make-celestial :pos (make-vec3 -1 0 2))
                          (make-celestial :pos (make-vec3 2 -10 -7))
                          (make-celestial :pos (make-vec3 4 -8 8))
                          (make-celestial :pos (make-vec3 3 5 -1))))

(defun and-fun (x y)
  (and x y))

(defun find-full-cycle (bodies)
  (declare (optimize (speed 3)))
  (labels ((adjust (h tl)
             (mapcar (lambda (o) (adjust-velocity h o)) tl)
             (when tl
               (adjust (car tl) (cdr tl)))))
    (let ((var   (mapcar #'copy-celestial bodies))
          (step  0))
      (do ()
          (nil)
        (adjust (car var) (cdr var))
        (mapcar #'move var)
        (incf step)
        (when (reduce #'and-fun (mapcar #'equalp var bodies))
          (return)))
      step)))
