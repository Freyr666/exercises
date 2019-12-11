(defconstant +input+ "./input")

(load "../intcode/intcode.lisp")

(use-package :aoc.intcode)

;; Part One
(defun change-direction (current turn)
  (case current
    ((:up)    (if (zerop turn) :left :right))
    ((:left)  (if (zerop turn) :down :up))
    ((:down)  (if (zerop turn) :right :left))
    ((:right) (if (zerop turn) :up :down))))

(defun change-coords (dir x y)
  (case dir
    ((:up) (values x (1+ y)))
    ((:left) (values (1- x) y))
    ((:right) (values (1+ x) y))
    ((:down) (values x (1- y)))))

(defun count-painted-once (stream)
  (let* ((chan    (make-chan))
         (running (eval-intcode-scheduled (parse-intcode stream) chan))
         (counter 0)
         (x       0)
         (y       0)
         (dir     :up)
         (table   (make-hash-table :test #'equal)))
    (do ()
        ((not running))
      (let* ((stored  (gethash (cons x y) table))
             (times   (if stored (car stored) 0))
             (color   (if stored (cdr stored) 0)))
        (setf running (funcall running color))
        (handler-case
            (progn
              (let ((new-color (read-chan chan))
                    (direction (change-direction dir (read-chan chan))))
                (multiple-value-bind (new-x new-y)
                    (change-coords direction x y)
                  (setf dir direction)
                  (setf (gethash (cons x y) table) (cons (1+ times) new-color))
                  (setf x new-x)
                  (setf y new-y))))
          (error () nil))))
    (maphash (lambda (k v) (when (not (zerop (car v)))
                             (incf counter)))
             table)
    counter))

(with-open-file (stream +input+)
  (count-painted-once stream))

;; Part Two

(defun paint-table (stream)
  (let* ((chan    (make-chan))
         (running (eval-intcode-scheduled (parse-intcode stream) chan))
         (counter 0)
         (x       0)
         (y       0)
         (dir     :up)
         (table   (make-hash-table :test #'equal)))
    ;; Add initial panel
    (setf (gethash (cons x y) table) 1)
    (do ()
        ((not running))
      (let* ((stored  (gethash (cons x y) table))
             (color   (if stored stored 0)))
        (setf running (funcall running color))
        (handler-case
            (progn
              (let ((new-color (read-chan chan))
                    (direction (change-direction dir (read-chan chan))))
                (multiple-value-bind (new-x new-y)
                    (change-coords direction x y)
                  (setf dir direction)
                  (setf (gethash (cons x y) table) new-color)
                  (setf x new-x)
                  (setf y new-y))))
          (error () nil))))
    table))

(defun render-picture (stream)
  (let ((table (paint-table stream)))
    (let ((min-x 0)
          (min-y 0)
          (max-x 0)
          (max-y 0))
      (maphash (lambda (k v) (let ((x (car k))
                                   (y (cdr k)))
                               (cond ((< x min-x) (setf min-x x))
                                     ((> x max-x) (setf max-x x))
                                     ((< y min-y) (setf min-y y))
                                     ((> y max-y) (setf max-y y)))))
               table)
      (let ((canvas (make-array (list (1+ (- max-x min-x))
                                      (1+ (- max-y min-y)))
                                :initial-element #\space)))
        (maphash (lambda (k v) (let ((x (- (car k)
                                           min-x))
                                     (y (- (cdr k)
                                           min-y)))
                                 (setf (aref canvas x y)
                                       (if (zerop v) #\space #\#))))
                 table)
        (loop
           for y from (- max-y min-y) downto 0
           do (progn
                (format t "~%")
                (loop
                   for x from 0 to (- max-x min-x)
                   do (format t "~a " (aref canvas x y)))))))))

(with-open-file (stream +input+)
  (render-picture stream))
