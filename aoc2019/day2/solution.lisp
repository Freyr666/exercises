(defconstant +input-path+ "./input")

(defun parse-char (char)
  (cond ((null char) nil)
        ((char= char #\,) :comma)
        ((and (char>= char #\0)
              (char<= char #\9)) (- (char-int char)
                                    (char-int #\0)))
        (t nil)))

(defun parse-intcode (stream)
  (labels ((rec (acc num-acc)
             (let ((symb (parse-char (read-char stream nil))))
               (cond
                 ((equal symb :comma)
                  (rec (cons num-acc acc) 0))
                 ((numberp symb)
                  (rec acc (+ (* num-acc 10)
                              symb)))
                 (t (cons num-acc acc))))))
    (apply #'vector (reverse (rec '() 0)))))

(defun eval-intcode (code)
  (let ((word-size 4))
    (labels ((decode-op (op)
               (case op
                 ((99) 'halt)
                 ((1)  '+)
                 ((2)  '*)
                 (t    (error op "Wrong opcode"))))
             (run (pc)
               (let ((op (decode-op (elt code pc))))
                 (case op
                   ((halt) code)
                   ((+ *)  (let ((arg-1-pos (elt code (+ pc 1)))
                                 (arg-2-pos (elt code (+ pc 2)))
                                 (result-pos (elt code (+ pc 3))))
                             (setf (elt code result-pos)
                                   (funcall op
                                            (elt code arg-1-pos)
                                            (elt code arg-2-pos)))
                             (run (+ pc word-size))))))))
      (run 0))))

(defun read-and-eval-intcode (stream)
  "Reads and evaluates a program
written in Intcode"
  (let ((program (parse-intcode stream)))
    (print program)
    (print (eval-intcode program))))

(defun read-fix-and-eval-intcode (stream)
  "Reads and evaluates a program
written in Intcode"
  (labels ((fix-alarm (code)
             (setf (elt code 1) 12)
             (setf (elt code 2) 2)))
    (let ((program (parse-intcode stream)))
      (fix-alarm program)
      (print program)
      (print (eval-intcode program)))))

;; Test
(with-input-from-string (stream "1,9,10,3,2,3,11,0,99,30,40,50")
  (read-and-eval-intcode stream))

;; Part One
(with-open-file (stream +input-path+)
  (read-fix-and-eval-intcode stream))

;; Part Two
(defun decompile-intcode (code)
  (let ((word-size 4))
    (labels ((decode-op (op)
               (case op
                 ((99) 'halt)
                 ((1)  '+)
                 ((2)  '*)
                 (t    (error op "Wrong opcode"))))
             (node-at-pos (pos)
               (if (numberp pos)
                   (elt code pos)
                   pos))
             (run (pc)
               (let ((op (decode-op (elt code pc))))
                 (case op
                   ((halt) (elt code 0))
                   ((+ *)  (let ((arg-1-pos (elt code (+ pc 1)))
                                 (arg-2-pos (elt code (+ pc 2)))
                                 (result-pos (elt code (+ pc 3))))
                             (setf (elt code result-pos)
                                   `(,op ,(node-at-pos arg-1-pos)
                                         ,(node-at-pos arg-2-pos)))
                             (run (+ pc word-size))))))))
      ;;(setf (elt code 1) :a)
      ;;(setf (elt code 2) :b)
      (run 0))))

(defun read-and-decompile-intcode (stream)
  "Decompiles intcode into a simple AST"
  (let ((program (parse-intcode stream)))
    (print program)
    (setf (elt program 1) 'noun)
    (setf (elt program 2) 'verb)
    (let ((res (decompile-intcode program)))
      res)))

(with-input-from-string (stream "1,9,10,3,2,3,11,0,99,30,40,50")
  (read-and-decompile-intcode stream))

(defun solve-task-2 ()
  (let ((decompiled-code (with-open-file (stream +input-path+)
                           (read-and-decompile-intcode stream))))
    (let ((result 0))
      (do ((a 0 (1+ a))) ;;TODO use loop macro
          ((or (= result 19690720)
               (= a 100)))
        (do ((b 0 (1+ b)))
            ((or (= result 19690720)
                 (= b 100)))
          (setf noun a
                verb b)
          (setf result (eval decompiled-code))))
      (list (+ (* 100 noun) verb) noun verb result))))
