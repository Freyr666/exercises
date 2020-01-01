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

;;
;; Simple nondeterministic computations
;;
(defun nondetp (x)
  (equal (car x)
         'nondet-tag))

(deftype nondet ()
  `(and list
        (satisfies nondetp)))

(declaim (ftype (function (t &rest list) nondet) either))
(defun either (first &rest others)
  (the nondet (cons 'nondet-tag (cons first others))))

(declaim (ftype (function (nondet) list) nondet->list))
(defun nondet->list (x)
  (cdr x))

(declaim (ftype (function (t) nondet) nondet-return))
(defun nondet-return (x)
  (either x))

(defun concat (list-of-lists)
  (when list-of-lists
    (let ((current (reverse (car list-of-lists)))
          (done    (concat (cdr list-of-lists))))
      (dolist (el current)
        (setf done (cons el done)))
      done)))

(declaim (ftype (function (list) nondet) concat-nondets))
(defun concat-nondets (lst)
  (cons 'nondet-tag
        (concat (mapcar #'cdr lst))))

(defmacro nondet-let (binding &body body)
  (let ((symb     (first binding))
        (variants (second binding)))
    `(concat-nondets
      (mapcar (lambda (v)
                (let ((res 
                       (let ((,symb v))
                         ,@body)))
                  (the nondet res)))
              ,variants))))

;;
;; End
;;

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
                (let ((reaction (list (intern res-name)
                                      (parse-integer res-mol)
                                      (mapcar (lambda (x) (cons (intern (cadr x))
                                                                (parse-integer (car x))))
                                              ingr))))
                  (setf reacts (cons reaction reacts)))))))
    reacts))

(defun find-min-ingredients (subst reactions)
  (labels ((find-min (pair)
             (destructuring-bind (name . mol) pair
               (let ((possible (remove-if-not (lambda (x) (eq (car x)
                                                              name))
                                              reactions)))
                 (->> possible
                      (mapcar (lambda (x)
                                (destructuring-bind (n m ings) x
                                  (let* ((coef (ceiling mol m))
                                         (new-m (* m coef))
                                         (adjusted-by-mol
                                          (mapcar (lambda (p)
                                                    (cons (car p)
                                                          (* (cdr p) coef)))
                                                  ings)))
                                    (find-min-ingredients (list n new-m adjusted-by-mol)
                                                          reactions)))))
                      (apply #'min)))))
           (get-min-ore (pair)
             (if (eq (car pair)
                     'ore)
                 (cdr pair)
                 (find-min pair))))
    (let ((needed (caddr subst)))
      (let ((res (->> needed
                      (mapcar #'get-min-ore)
                      (apply #'+))))
        (format t "~A needs ~d ORE~%" subst res)
        res))))

;; Subst : (name molar-mass ingreds) -> reactions -> int
(defun evaluate-needed-ore (subst reactions)
  '())

(with-open-file (stream +input+)
  (let ((reactions (parse-reactions stream)))
    (->> reactions
         (remove-if (lambda (x) (not (eq (car x)
                                         'fuel))))
         car
         (lambda (f) (find-min-ingredients f reactions)))))
  
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
    (->> reactions
         (remove-if (lambda (x) (not (eq (car x)
                                         'fuel))))
         car
         (lambda (f) (find-min-ingredients f reactions)))))

;; Test 165
(with-input-from-string (stream "9 ORE => 2 A
8 ORE => 3 B
7 ORE => 5 C
3 A, 4 B => 1 AB
5 B, 7 C => 1 BC
4 C, 1 A => 1 CA
2 AB, 3 BC, 4 CA => 1 FUEL")
  (let ((reactions (parse-reactions stream)))
    (->> reactions
         (remove-if (lambda (x) (not (eq (car x)
                                         'fuel))))
         car
         (lambda (f) (find-min-ingredients f reactions)))))
