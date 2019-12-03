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

(defun directions->sorted-trace (dir-list)
  (let ((res (make-hash-table :size 1000))
        (x   0)
        (y   0)
        (l   0))
    (labels ((move (dim inc dist)
               (dotimes (i 
      (dolist (el dir-list)
        (case (car el)
          ((:u) (move 'y #'incf (cdr el)))
          ((:d) (move 'y #'decf (cdr el)))
          ((:r) (move 'x #'incf (cdr el)))
          ((:l) (move 'x #'decf (cdr el)))
                         
        

(defun solution-for-intersection ()
  (with-open-file (stream "./input")
    (let* ((pair   (read-wires-paths stream))
           (wire-a (car pair))
           (wire-b (cdr pair)))
      (let ((sorted-trace-a (
