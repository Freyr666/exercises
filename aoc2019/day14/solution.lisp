(defconstant +input+ "./input")

(defmacro ->> (expr &rest exps)
  "A simple piping macro, works with lambdas,
functions, symbols or bodies.

Example:
  (->> 1 
       1+
       #'1+
       (lambda (x) (+ x 1))
       (+ 2 3 4))
would be rewritten as
  (+ 2 3 4 (FUNCALL (LAMBDA (X) (+ X 1)) (FUNCALL #'1+ (FUNCALL #'1+ 1))))"
  (let ((body (reduce (lambda (body exp)
                        (cond ((and (listp exp)      ;; Quoted function
                                    (eq (car exp)
                                        'quote))
                               (list 'funcall `(function ,exp) body))
                              ((and (listp exp)      ;; Function
                                    (or (eq (car exp)
                                            'function)
                                        (eq (car exp)
                                            'lambda)))
                               (list 'funcall exp body))
                              ((listp exp)           ;; Pure body
                               `(,(car exp) ,@(cdr exp) ,body))
                              ((functionp exp)
                               (list 'funcall exp body))
                              ((symbolp exp)
                               (list 'funcall `(function ,exp) body))
                              (t
                               (error "->: Bad function"))))
                      exps
                      :initial-value expr)))
    `(,@body)))

(defun make-ingred (name mass ingreds)
  (list name mass ingreds))

(defun ingred-name (ingred)
  (car ingred))

(defun ingred-mass (ingred)
  (cadr ingred))

(defun ingred-list (ingred)
  (caddr ingred))

(defun split-string (string &optional (delim " "))
  (let ((toks '())
        (cur  (make-string-output-stream))
        (delim-pos 0))
    (labels ((normal-read (pos)
               (cond ((>= pos (length string))
                      (result))
                     ((char= (aref string pos)
                             (aref delim delim-pos))
                      (skip-delim pos pos))
                     (t
                      (write-char (aref string pos) cur)
                      (normal-read (1+ pos)))))
             (skip-delim (stored-pos pos)
               (cond ((>= pos (length string))
                      (result))
                     ((>= delim-pos (length delim))
                      (setf delim-pos 0)
                      (setf toks (cons (get-output-stream-string cur) toks))
                      (normal-read pos))
                     ((char= (aref string pos)
                             (aref delim delim-pos))
                      (incf delim-pos)
                      (skip-delim stored-pos (1+ pos)))
                     (t
                      (setf delim-pos 0)
                      (ignore-delim stored-pos))))
             (ignore-delim (pos)
               (cond ((>= pos (length string))
                      (result))
                     (t
                      (write-char (aref string pos) cur)
                      (normal-read (1+ pos)))))
             (result ()
               (remove-if (lambda (s) (= (length s)
                                         0))
                          (reverse (cons (get-output-stream-string cur) toks)))))
      (normal-read 0))))

(defun parse-reactions (stream)
  (let ((reacts '()))
    (loop
       for line = (read-line stream nil nil)
       until (null line)
       do (destructuring-bind (f ts)
              (split-string line "=>")
            (let* ((raw (split-string f ","))
                   (ingr (mapcar #'split-string raw))
                   (res  (split-string ts)))
              (destructuring-bind (res-mol res-name) res
                (let ((reaction (make-ingred (intern res-name)
                                             (parse-integer res-mol)
                                             (mapcar (lambda (x) (cons (intern (cadr x))
                                                                       (parse-integer (car x))))
                                                     ingr))))
                  (setf reacts (cons reaction reacts)))))))
    reacts))

(defun create-reaction-table (list)
  (let ((table (make-hash-table :test 'equal)))
    (dolist (entry list)
      (destructuring-bind (n q lst) entry
        (setf (gethash n table)
              (cons q lst))))
    table))

(defun scale-molar-mass (factor ingreds)
  (mapcar (lambda (p) (destructuring-bind (n . m) p
                        (cons n (* factor m))))
          ingreds))

(defun find-min-ingredients (reactions &optional (stock (make-hash-table :test 'equal)))
  (let ((table (create-reaction-table reactions)))
    (labels ((calc (name required-mass)
               (let ((in-stock (min required-mass
                                    (or (gethash name stock) 0))))
                 (decf required-mass in-stock)
                 (when (gethash name stock)
                   (decf (gethash name stock) in-stock)))
               (cond ((equal 'ORE name)   required-mass)
                     ((= required-mass 0) 0)
                     (t (destructuring-bind (amount . ingreds) (gethash name table)
                          (let* ((factor    (ceiling required-mass amount))
                                 (obtained  (* factor amount))
                                 (excessive (- obtained required-mass))
                                 (cost      (reduce #'+
                                                    (mapcar (lambda (pair)
                                                              (calc (car pair) (cdr pair)))
                                                            (scale-molar-mass factor ingreds)))))
                            (setf (gethash name stock)
                                  (+ excessive
                                     (or (gethash name stock) 0)))
                            cost))))))
      (calc 'FUEL 1))))
                                    
;; Subst : (name molar-mass ingreds) -> reactions -> int
(defun evaluate-needed-ore (subst reactions)
  '())

(with-open-file (stream +input+)
  (let ((reactions (parse-reactions stream)))
    (find-min-ingredients reactions)))
  
;; Test 180697
(with-input-from-string (stream "2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG
17 NVRVD, 3 JNWZP => 8 VPVL
53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL
22 VJHF, 37 MNCFX => 5 FWMGM
139 ORE => 4 NVRVD
144 ORE => 7 JNWZP
5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC
5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV
145 ORE => 6 MNCFX
1 NVRVD => 8 CXFTF
1 VJHF, 6 MNCFX => 4 RFSQX
176 ORE => 6 VJHF")
  (let ((reactions (parse-reactions stream)))
    (find-min-ingredients reactions)))
