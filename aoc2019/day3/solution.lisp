(defconstant +input-path+ "./input")

(defstruct coord
  (x-1 0 :type integer)
  (y-1 0 :type integer)
  (x-2 0 :type integer)
  (y-2 0 :type integer)
  (dist 0 :type integer))

(defstruct intersect-point
  (x 0 :type integer)
  (y 0 :type integer)
  (surplus 0 :type integer))

(defmethod simple-intersection ((horiz coord) (vert coord))
  ;; TODO these should be proper coord subclasses verified
  ;; on initialization
  (assert (= (coord-y-1 horiz) (coord-y-2 horiz))
          '()
          "First value should be horizontal ~A" horiz)
  (assert (= (coord-x-1 vert) (coord-x-2 vert))
          '()
          "First value should be vertical ~A" vert)
  (labels ((between (arg v1 v2)
             (and (>= arg
                      (min v1 v2))
                  (<= arg
                      (max v1 v2)))))
    (if (and (between (coord-y-1 horiz)
                      (coord-y-1 vert)
                      (coord-y-2 vert))
             (between (coord-x-1 vert)
                      (coord-x-1 horiz)
                      (coord-x-2 horiz)))
        (make-intersect-point :x (coord-x-1 vert)
                              :y (coord-y-1 horiz)
                              :surplus (+ (abs (- (coord-x-2 horiz) (coord-x-1 vert)))
                                          (abs (- (coord-y-2 vert) (coord-y-1 horiz)))))
        nil)))

(defun parse-direction (str)
  (let ((distance (parse-integer str :start 1)))
    (case (aref str 0)
      ((#\L #\l) (cons :l distance))
      ((#\R #\r) (cons :r distance))
      ((#\U #\u) (cons :u distance))
      ((#\D #\d) (cons :d distance))
      (t         (error "Bad direction designator")))))

(defun read-wires-paths (stream)
  (labels ((cons-if-string (stream lst)
             (let ((s (get-output-stream-string stream)))
               (if (string-not-equal s "")
                   (cons s lst)
                   lst)))
           (parse (acc string-acc)
             (let ((char (read-char stream nil nil)))
               (case char
                 ((#\newline nil) (reverse (cons-if-string string-acc acc)))
                 ((#\,)           (parse (cons-if-string string-acc acc)
                                         (make-string-output-stream)))
                 ((#\space #\tab) (parse acc string-acc))
                 (t               (progn
                                    (write-char char string-acc)
                                    (parse acc string-acc)))))))
    (cons (mapcar #'parse-direction
                  (parse '() (make-string-output-stream)))
          (mapcar #'parse-direction
                  (parse '() (make-string-output-stream))))))

(defun directions->paths (dir-list)
  (let ((horiz '())
        (vert  '())
        (x   0)
        (y   0)
        (steps 0))
    (labels ((move-x (f by)
               (let ((start-x x)
                     (start-y y))
                 (setf x (funcall f x by))
                 (setf steps (+ steps by))
                 (setf horiz (cons (make-coord :x-1 start-x
                                               :y-1 start-y
                                               :x-2 x
                                               :y-2 y
                                               :dist steps)
                                   horiz))))
             (move-y (f by)
               (let ((start-x x)
                     (start-y y))
                 (setf y (funcall f y by))
                 (setf steps (+ steps by))
                 (setf vert (cons (make-coord :x-1 start-x
                                               :y-1 start-y
                                               :x-2 x
                                               :y-2 y
                                               :dist steps)
                                  vert)))))
      (dolist (el dir-list)
        (case (car el)
          ((:u) (move-y #'+ (cdr el)))
          ((:d) (move-y #'- (cdr el)))
          ((:r) (move-x #'+ (cdr el)))
          ((:l) (move-x #'- (cdr el)))))
      (cons horiz vert))))

(defun intersections (horiz vert)
  (let ((res '()))
    (dolist (h horiz)
      (dolist (v vert)
        (let ((i (simple-intersection h v)))
          (when i
            (let ((total-steps (- (+ (coord-dist h)
                                     (coord-dist v))
                                  (intersect-point-surplus i))))
              (setf res (cons (cons i total-steps) res)))))))
    res))
                         
(defun find-wire-intersections (stream)
  (labels ((point-dist (point)
             (+ (intersect-point-x point)
                (intersect-point-y point))))
    (let* ((pair   (read-wires-paths stream))
           (wire-a (directions->paths (car pair)))
           (wire-b (directions->paths (cdr pair)))
           (a-horiz (car wire-a))
           (a-vert  (cdr wire-a))
           (b-horiz (car wire-b))
           (b-vert  (cdr wire-b)))
      (apply #'min
             (mapcar (lambda (p) (point-dist (car p)))
                     (append (intersections a-horiz b-vert)
                             (intersections b-horiz a-vert)))))))

(with-open-file (stream "./input")
  (find-wire-intersections stream))

;; Part Two
(defun find-intersection-lowest-steps (stream)
  (labels ((point-dist  (point)
             (+ (intersect-point-x point)
                (intersect-point-y point)))
           (smallest (smaller lst)
             (when lst
               (let ((res (car lst)))
                 (dolist (el (cdr lst))
                   (when (funcall smaller el res)
                     (setf res el)))
                 res))))
    (let* ((pair   (read-wires-paths stream))
           (wire-a (directions->paths (car pair)))
           (wire-b (directions->paths (cdr pair)))
           (a-horiz (car wire-a))
           (a-vert  (cdr wire-a))
           (b-horiz (car wire-b))
           (b-vert  (cdr wire-b)))
      (let ((res (smallest (lambda (l r) (< (cdr l) (cdr r)))
                           (append (intersections b-horiz a-vert)
                                   (intersections a-horiz b-vert)))))
        (when res
          (cdr res))))))

(with-open-file (stream "./input")
  (find-intersection-lowest-steps stream))
