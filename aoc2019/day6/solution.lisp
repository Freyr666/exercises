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

(with-open-file (stream +input+)
  (parse-orbits stream))
