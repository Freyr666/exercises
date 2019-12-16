(defconstant +input+ "./input")

(load "../intcode/intcode.lisp")

(use-package :aoc.intcode)

(defclass screen ()
  ((tiles :type '(simple-array (* *) fixnum))))

(defmethod initialize-instance :after ((s screen) &key)
  (setf (slot-value s 'tiles)
        (make-array (list 25 40) :element-type 'fixnum)))

(defun adjust-screen-size (s x y)
  (declare (type screen s)
           (type integer x y)
           (optimize (speed 3) (debug 0)))
  (let ((y-dim (array-dimension (slot-value s 'tiles) 0))
        (x-dim (array-dimension (slot-value s 'tiles) 1)))
    (when (or (> x x-dim)
              (> y y-dim))
      (let ((new-tiles (make-array (list y x)
                                   :element-type 'fixnum)))
        (dotimes (y-ind y-dim)
          (dotimes (x-ind x-dim)
            (setf (aref new-tiles y-ind x-ind)
                  (aref (slot-value s 'tiles) y-ind x-ind))))
        (setf (slot-value s 'tiles)
              new-tiles)))))

(defconstant +empty-tile+ 0)
(defconstant +wall-tile+ 1)
(defconstant +block-tile+ 2)
(defconstant +paddle-tile+ 3)
(defconstant +ball-tile+ 4)

(defmethod draw-tile ((s screen) (x integer) (y integer) (tile fixnum))
  (handler-case
      (setf (aref (slot-value s 'tiles) y x)
            tile)
    (error ()
      (progn
        (adjust-screen-size s x y)
        (draw-tile x y tile)))))

(defmethod read-tile ((s screen) (x integer) (y integer))
  (handler-case
      (aref (slot-value s 'tiles) y x)
    (error ()
      (progn
        (adjust-screen-size s x y)
        (read-tile x y)))))

(defmethod x-dimension ((s screen))
  (array-dimension (slot-value s 'tiles) 1))

(defmethod y-dimension ((s screen))
  (array-dimension (slot-value s 'tiles) 0))

;; Part One

(defun split-by (list num)
  (labels ((rec (acc cur n lst)
             (cond ((null lst)
                    (reverse (delete-if (lambda (el) (< (length el)
                                                        num))
                                        (cons (reverse cur) acc))))
                   ((zerop (mod n num))
                    (rec (cons (reverse cur) acc)
                         (list (car lst))
                         (1+ n)
                         (cdr lst)))
                   (t
                    (rec acc
                         (cons (car lst) cur)
                         (1+ n)
                         (cdr lst))))))
    (rec '() '() 0 list)))

(defun count-blocks (stream)
  (let ((result (eval-intcode-buffered (parse-intcode stream)
                                       '()))
        (screen (make-instance 'screen)))
    (dolist (inst (split-by result 3))
      (let ((x  (first inst))
            (y  (second inst))
            (id (third inst)))
        (draw-tile screen x y id)))
    (let ((blocks 0))
      (dotimes (y-ind (y-dimension screen))
        (dotimes (x-ind (x-dimension screen))
          (when (= (read-tile screen x-ind y-ind)
                   +block-tile+)
            (incf blocks))))
      (values blocks screen))))

(with-open-file (stream +input+)
  (count-blocks stream))

;; Part Two

(defconstant +num-of-continues+ 2)

(defconstant +same+ 0)

(defconstant +left+ -1)

(defconstant +right+ 1)

(defun drain-chan (chan)
  (loop
     while (not (chan-empty-p chan))
     for c = (read-chan chan)
     collect c))

(defun find-ball (screen)
  (let (res)
    (dotimes (y-ind (y-dimension screen))
      (dotimes (x-ind (x-dimension screen))
        (when (= (read-tile screen x-ind y-ind)
                 +ball-tile+)
          (setf res (cons x-ind y-ind))
          (return))))
    res))

(defun find-pad (screen)
  (let (res)
    (dotimes (y-ind (y-dimension screen))
      (dotimes (x-ind (x-dimension screen))
        (when (= (read-tile screen x-ind y-ind)
                 +paddle-tile+)
          (setf res (cons x-ind y-ind))
          (return))))
    res))

(defvar *next-projected-impact* nil)
(setq *next-projected-impact* nil)

(defun add-coord (ball vect)
  (cons (+ (car ball)
           (car vect))
        (+ (cdr ball)
           (cdr vect))))

(defun bouncy-body (screen x y)
  (case (read-tile screen x y)
    ;; +wall-tile+ +block-tile+ +paddle-tile+
    ((1 2) t)
    (t nil)))

;; (defun project-impact-point (screen pad-y ball ball-prev)
;;   (let ((vect (cons (- (car ball)
;;                        (car ball-prev))
;;                     (- (cdr ball)
;;                        (cdr ball-prev))))
;;         (pos  (cons (car ball)
;;                     (cdr ball))))
;;     (labels ((update-vect ()
;;                (or (update-vect-x)
;;                    (update-vect-y)))
;;              (update-vect-x ()
;;                (when (or (and (< (car vect) 0)
;;                               (bouncy-body screen
;;                                            (1- (car pos))
;;                                            (cdr pos)))
;;                          (and (> (car vect) 0)
;;                               (bouncy-body screen
;;                                            (1+ (car pos))
;;                                            (cdr pos))))
;;                  (setf vect (cons (- (car vect))
;;                                   (cdr vect)))
;;                  t))
;;              (update-vect-y ()
;;                (when (or (and (< (cdr vect) 0)
;;                               (bouncy-body screen
;;                                            (car pos)
;;                                            (1- (cdr pos))))
;;                          (and (> (cdr vect) 0)
;;                               (bouncy-body screen
;;                                            (car pos)
;;                                            (1+ (cdr pos)))))
;;                  (setf vect (cons (car vect)
;;                                   (- (cdr vect))))
;;                  t)))
;;       (do ()
;;           (nil)
;;         (update-vect)
;;         (setf pos (add-coord pos vect))
;;         (when (>= (cdr pos)
;;                   (1- pad-y))
;;           (setf *next-projected-impact* (car pos))
;;           (return))))))

(defun project-impact-point (screen pad-y ball ball-prev)
  (let ((vect-x (- (car ball)
                   (car ball-prev)))
        (vect-y (- (cdr ball)
                   (cdr ball-prev)))
        (pos-x  (car ball))
        (pos-y  (cdr ball)))
    (when (and (> vect-y 0)
               (< pos-y (1- pad-y)))
      (loop
         until (>= pos-y (1- pad-y))
         do (setf pos-y (+ pos-y vect-y)
                  pos-x (+ pos-x vect-x)))
      (setf *next-projected-impact* pos-x))))

;; (defun preferred-pad-direction (screen ball ball-prev)
;;   (let* ((pad   (find-pad screen))
;;          (pad-y (cdr pad))
;;          (pad-x (car pad)))
;;     ;;(when (or (null *next-projected-impact*)
;;     ;;          (= (cdr ball)
;;     ;;             (- pad-y 2)))
;;     (when (not (= (cdr ball)
;;                   (1- pad-y)))
;;       (project-impact-point screen pad-y ball ball-prev))
;;     (cond ((null *next-projected-impact*)
;;            (error "Can't figure out ball's trajectory"))
;;           ((= *next-projected-impact* pad-x)
;;            +same+)
;;           ((< *next-projected-impact* pad-x)
;;            +left+)
;;           ((> *next-projected-impact* pad-x)
;;            +right+))))

(defun preferred-pad-direction (screen ball ball-prev)
  (let* ((pad   (find-pad screen))
         (pad-y (cdr pad))
         (pad-x (car pad)))
    ;;(when (or (null *next-projected-impact*)
    ;;          (= (cdr ball)
    ;;             (- pad-y 2)))
    (when (not (= (cdr ball)
                  (1- pad-y)))
      (project-impact-point screen pad-y ball ball-prev))
    (cond ((null *next-projected-impact*)
           (error "Can't figure out ball's trajectory"))
          ((= *next-projected-impact* pad-x)
           +same+)
          ((< *next-projected-impact* pad-x)
           +left+)
          ((> *next-projected-impact* pad-x)
           +right+))))

(defun score-p (inst)
  (and (= (first inst) -1)
       (= (second inst) 0)))

(defun render (screen)
  (dotimes (y (y-dimension screen))
    (format t "~%")
    (dotimes (x (x-dimension screen))
      (format t "~a" (case (read-tile screen x y)
                       (0 " ")
                       (1 "#")
                       (2 "*")
                       (3 "_")
                       (4 "0"))))))

(defun autoplay-the-game (stream)
  (let ((code (parse-intcode stream))
        (chan (make-chan)))
    (setf (aref code 0)           +num-of-continues+
          *next-projected-impact* nil)
    (let ((running (eval-intcode-scheduled code chan))
          (screen  (make-instance 'screen))
          (score   0)
          (ball-stored nil))
      ;; Frameloop
      (do ()
          ((not running))
        (let ((inst (split-by (drain-chan chan) 3)))
          ;; Redraw the screen
          (dolist (i inst)
            (if (score-p i)
                (setf score (third i))
                (draw-tile screen
                           (first i)
                           (second i)
                           (third i))))
          ;; Update the pad
          (render screen)
          (format t "Next projected impact: ~a~%" *next-projected-impact*)
          (format t "Pad position: ~a~%" (find-pad screen))
          (let ((ball   (find-ball screen)))
            (format t "Ball: ~a~%" ball)
            (if (and ball ball-stored)
                (let ((dir (preferred-pad-direction screen ball ball-stored)))
                  (format t "Dir: ~a~%" dir)
                  (setf ball-stored ball)
                  (setf running (funcall running dir)))
                (progn
                  (when ball
                    (setf ball-stored ball))
                  (setf running (funcall running +same+)))))))
      (values score screen))))

(with-open-file (stream +input+)
  (autoplay-the-game stream))