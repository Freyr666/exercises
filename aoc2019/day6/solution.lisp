(defconstant +input+ "./input")

(defun parse-single-orbit (stream)
  (let ((first  (make-string-output-stream))
        (second (make-string-output-stream)))
    (labels ((first-planet ()
               (let ((symb (read-char stream nil nil)))
                 (case symb
                   ((nil) nil)
                   ((#\)) (second-planet))
                   (t     (progn
                            (write-char symb first)
                            (first-planet))))))
             (second-planet ()
               (let ((symb (read-char stream nil nil)))
                 (case symb
                   ((nil #\newline) t)
                   (t     (progn
                            (write-char symb second)
                            (second-planet)))))))
      (when (first-planet)
        (let ((f (get-output-stream-string first))
              (s (get-output-stream-string second)))
          (when (and (string/= f "")
                     (string/= s ""))
            (cons f s)))))))
                                    
(defun parse-orbits (stream)
  (labels ((pair->symb (p)
             (destructuring-bind (f . s) p
               (cons (read-from-string (concatenate 'string "planet-" f))
                     (read-from-string (concatenate 'string "planet-" s)))))
           (rec (acc)
             (let ((pair (parse-single-orbit stream)))
               (if pair
                   (rec (cons (pair->symb pair)
                              acc))
                   (reverse acc)))))
    (rec '())))

(defun read-orbit-tree (stream)
  (let ((orbits (parse-orbits stream)))
    (labels ((compose-tree (symb pairs)
               (let ((children (mapcar #'cdr
                                       (remove-if-not (lambda (x) (equal (car x)
                                                                         symb))
                                                      pairs)))
                     (rest     (remove-if (lambda (x) (equal (car x)
                                                             symb))
                                          pairs)))
                 (cons symb (mapcar (lambda (s) (compose-tree s rest))
                                    children)))))
      (compose-tree 'planet-com orbits))))

(defun compute-checksum (planet-tree)
  (labels ((traverse (prev-orbits tree)
             (reduce #'+
                     (mapcar (lambda (stree) (traverse (1+ prev-orbits)
                                                       stree))
                             (cdr tree))
                     :initial-value prev-orbits)))
    (traverse 0 planet-tree)))

;;Test
(with-input-from-string (stream "COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L")
 (compute-checksum (read-orbit-tree stream)))

(with-open-file (stream +input+)
  (compute-checksum (read-orbit-tree stream)))

;; Part two

(defun evaluate-path-you->santa (tree)
  (labels ((traverse (tree)
             (let ((path-to-you nil)
                   (path-to-san nil)
                   (paths   (cdr tree))
                   (current (car tree)))
               (cond ((equal current
                             'planet-you)
                      (values nil (cons :you 0)))
                     ((equal current
                             'planet-san)
                      (values nil (cons :san 0)))
                     ((null paths)
                      (values nil nil))
                     (t
                      (let ((res
                             (loop
                                for sub in paths
                                do (multiple-value-bind (res single) (traverse sub)
                                     (cond (res
                                            (return res))
                                           ((and single
                                                 (equal (car single)
                                                        :you))
                                            (setf path-to-you (1+ (cdr single))))
                                           ((and single
                                                 (equal (car single)
                                                        :san))
                                            (setf path-to-san (1+ (cdr single)))))))))
                        (cond (res
                               (values res nil))
                              ((and path-to-you
                                    path-to-san)
                               (values (+ path-to-you
                                          path-to-san)
                                       nil))
                              (path-to-you
                               (values nil (cons :you path-to-you)))
                              (path-to-san
                               (values nil (cons :san path-to-san)))
                              (t
                               (values nil nil)))))))))
    (- (traverse tree)
       2)))

;; Test

(with-input-from-string (stream "COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L
K)YOU
I)SAN")
 (evaluate-path-you->santa (read-orbit-tree stream)))

(with-open-file (stream +input+)
  (evaluate-path-you->santa (read-orbit-tree stream)))
