(defconstant +input+ "./input")

(defun parse-asteroids (stream)
  (let ((asteroids '())
        (x 0)
        (y 0))
    (labels ((parse ()
               (let ((char (read-char stream nil nil)))
                 (when char
                   (case char
                     ((#\newline) (progn
                                    (incf y)
                                    (setf x 0)
                                    (parse)))
                     ((#\.)       (progn
                                    (incf x)
                                    (parse)))
                     ((#\#)       (progn
                                    (setf asteroids
                                          (cons (cons x y) asteroids))
                                    (incf x)
                                    (parse))))))))
      (parse)
      asteroids)))

(defun distance (point-1 point-2)
  (destructuring-bind (x-1 . y-1) point-1
    (destructuring-bind (x-2 . y-2) point-2
      (+ (abs (- x-1 x-2))
         (abs (- y-1 y-2))))))

(defun vector-col (center p-1 p-2)
  (destructuring-bind (xc . yc) center
    (destructuring-bind (x1 . y1) p-1
      (destructuring-bind (x . y) p-2
        (and (if (= xc x1) ;; Are on the same line
                 (= x xc)
                 (let* ((a   (/ (- y1 yc)
                                (- x1 xc)))
                        (b   (- yc
                                (* a xc))))
                   (= (+ (* a x)
                         b)
                      y)))
             (> (+ (* (- x1 xc) ;; Dot product > 0
                      (- x xc))
                   (* (- y1 yc)
                      (- y yc)))
                0)
             (> (distance center p-2) ;; P2 is further
                (distance center p-1)))))))

(defun sort-distance (point lst)
  (sort (copy-seq lst) (lambda (coord-1 coord-2)
                         (< (distance point coord-1)
                            (distance point coord-2)))))

(defun filter-invis (center p lst)
  (remove-if (lambda (o) (vector-col center p o))
             lst))

(defun count-visible (asteroids)
  (let ((annotated '()))
    (labels ((traverse (counter point lst)
               (if (null lst)
                   counter
                   (let ((p (car lst)))
                     (traverse (1+ counter)
                               point
                               (sort-distance point
                                              (filter-invis point p (cdr lst)))))))
             (recur (acc lst)
               (when lst
                 (let* ((cur  (car lst))
                        (rest (sort-distance cur
                                             (append acc
                                                     (cdr lst)))))
                   (setf annotated
                         (cons (cons cur (traverse 0 cur rest))
                               annotated))
                   (recur (cons cur acc)
                          (cdr lst))))))
      (recur '() asteroids)
      (let (coord
            (max 0))
        (dolist (i annotated)
          (when (> (cdr i)
                   max)
            (setf coord (car i))
            (setf max (cdr i))))
        (values coord max)))))

(with-open-file (stream +input+)
  (count-visible (parse-asteroids stream)))

;; Part Two

(defvar base-coord
  (with-open-file (stream +input+)
    (count-visible (parse-asteroids stream))))

(defun partition (f list)
  (let (l r)
    (dolist (item list)
      (if (funcall f item)
          (setf l (cons item l))
          (setf r (cons item r))))
    (cons l r)))

(defun partition-invis (center p lst)
  "Result is ((invisible) . (visible))"
  (partition (lambda (o) (vector-col center p o)) lst))

(defvar *pi* (* (atan 1) 4))

(defun degree (vec)
  (destructuring-bind ((x1 . y1) . (x2 . y2))
      vec
    (if (> x2 x1)
        (let* ((x (- x2 x1))
               (y (- y1 y2))) ;; Up is actually under lower Y
          (acos (/ y
                   (sqrt (+ (* x x)
                            (* y y))))))
        (let* ((x (- x1 x2))
               (y (- y2 y1))) ;; Up is actually under lower Y
          (+ (acos (/ y
                      (sqrt (+ (* x x)
                               (* y y)))))
             *pi*)))))
  
(defun sort-clockwise (base points)
  (flet ((pred (p-1 p-2)
           (< (degree (cons base p-1))
              (degree (cons base p-2)))))
    (sort (copy-seq points) #'pred)))

(defun find-200th-asteroid (base-coord asteroids)
  (let ((rest       (delete-if (lambda (x) (equal base-coord x))
                               asteroids))
        (stages     '()))
    (print (mapcar (lambda (a) (cons a (degree (cons base-coord a)))) asteroids))
    (labels ((traverse (stage invis lst)
               (if (null lst)
                   (values stage (apply #'append invis))
                   (let* ((p     (car lst))
                          (part  (partition-invis base-coord p (cdr lst)))
                          (inv   (car part))
                          (vis   (cdr part)))
                     (traverse (cons p stage)
                               (cons inv invis)
                               (sort-distance base-coord vis)))))
             (recur (rest)
               (multiple-value-bind (stage invis)
                   (traverse '() '() rest)
                 (setf stages (cons stage stages))
                 (when invis
                   (recur invis)))))
      (recur rest)
      (setf stages (reverse stages))
      (let ((pos 200)
            (c 0)
            stage)
        (dolist (s stages)
          (let ((l (length s)))
            (if (< l pos)
                (setf pos (- pos l))
                (progn
                  (setf stage s)
                  (return)))))
        (setf stage (sort-clockwise base-coord stage))
        (values (nth pos stage) pos)))))

(with-open-file (stream +input+)
  (find-200th-asteroid base-coord (parse-asteroids stream)))
