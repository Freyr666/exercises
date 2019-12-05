(defconstant +input+ '(109165 . 576723))

(defun range->bounds (range)
  (destructuring-bind (first . second) range
    (when (< first second)
      (labels ((num->list (acc num)
                 (if (zerop num)
                     acc
                     (multiple-value-bind (v r)
                         (truncate num 10)
                       (num->list (cons r acc) v)))))
        (let ((lower (num->list '() first))
              (upper (num->list '() second)))
          (let ((diff (- (length upper)
                         (length lower))))
            (mapcar #'cons
                    (append (make-list diff :initial-element 0)
                            lower)
                    upper)))))))
        
(defun enumerate (doubled-p bounds acc could-carry)
  (if (not bounds)
      (if (funcall doubled-p acc)
          1
          0)
      (let ((lower   (caar bounds))
            (upper   (cdar bounds))
            (rest    (cdr bounds))
            (sum     0))
        (loop
           for d from lower to upper
           do (let* ((still-could-carry (and could-carry
                                             (= d upper)))
                     (new-lower-bounds (mapcar (lambda (p) (cons d
                                                                 (cdr p)))
                                               rest))
                     (new-bounds       (if still-could-carry
                                           new-lower-bounds
                                           (mapcar (lambda (p) (cons (car p) 9))
                                                   new-lower-bounds))))
                (setf sum (+ sum
                             (enumerate doubled-p
                                        new-bounds
                                        (cons d acc)
                                        still-could-carry)))))
        sum)))

(defun doubled-p (lst)
  (let ((res nil)
        (prev nil))
    (dolist (d lst)
      (setf res (or res
                    (equal prev d))
            prev d))
    res))

(defun enumerate-combinations ()
  (enumerate #'doubled-p (range->bounds +input+) nil t))

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
  (enumerate #'doubled-no-adjacent-p (range->bounds +input+) nil t))
