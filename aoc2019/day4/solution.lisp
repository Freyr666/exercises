(defconstant +input+ '(109165 . 576723))

(defconstant +borders+
  (let ((res '())
        (first (car +input+))
        (second (cdr +input+)))
    (loop
       while (not (or (zerop first)
                      (zerop second)))
       do
         (multiple-value-bind (fv fr) (truncate first 10)
           (multiple-value-bind (sv sr) (truncate second 10)
             (setf first fv
                   second sv
                   res (cons (cons fr sr) res)))))
    res))

(defun doubled-p (lst)
  (let ((res nil)
        (prev nil))
    (dolist (d lst)
      (setf res (or res
                    (equal prev d))
            prev d))
    res))

(defun enumerate (doubled-p bounds acc minimal max-digit)
  (let ((sum 0))
    (if (not bounds)
        (if (funcall doubled-p acc)
            1
            0)
        (let ((current (car bounds))
              (rest    (cdr bounds)))
          (loop
             for d from (max minimal (car current)) to (cdr current)
             do (let* ((new-lower-bounds (if (= d (car current))
                                             rest
                                             (mapcar (lambda (p) (cons 0
                                                                       (cdr p)))
                                                     rest)))
                       (new-bounds       (if (and max-digit
                                                  (= d (cdr current)))
                                             new-lower-bounds
                                             (mapcar (lambda (p) (cons (car p)
                                                                       9))
                                                     new-lower-bounds))))
                  (setf sum (+ sum
                               (enumerate doubled-p
                                          new-bounds
                                          (cons d acc)
                                          (max minimal d)
                                          (and max-digit
                                               (= d (cdr current))))))))
          sum))))

(defun enumerate-combinations ()
  (enumerate #'doubled-p +borders+ nil 0 t))

;; Part Two
  
(defun doubled-no-adjacent-p (lst)
  (labels ((split-by-similar (acc similar lst)
             (if (not lst)
                 (cons similar acc)
                 (if (or (not similar)
                         (equal (car similar)
                                (car lst)))
                     (split-by-similar acc
                                       (cons (car lst) similar)
                                       (cdr lst))
                     (split-by-similar (cons similar acc)
                                       (list (car lst))
                                       (cdr lst))))))
    (and (find-if (lambda (l) (= (length l) 2)) (split-by-similar '() '() lst))
         t)))

(defun enumerate-combinations-enhanced ()
  (enumerate #'doubled-no-adjacent-p +borders+ nil 0 t))
