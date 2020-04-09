(defun mappend (f list)
  (apply #'append (mapcar f list)))

(defun random-elt (list)
  (elt list (random (length list))))

(defparameter *simple-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (article noun))
    (verb-phrase -> (verb noun-phrase))
    (article -> the a)
    (noun -> man ball woman table)
    (verb -> hit took saw liked)))

(defvar *grammar* *simple-grammar*
  "The grammar used by GENERATE. Initially, this is
 *simple-grammar*, but we can switch to other ones.")

(defun rule-lhs (rule)
  "The left-hand side of a rule."
  (first rule))

(defun rule-rhs (rule)
  "The right-hand side of a rule."
  (rest (rest rule)))

(defun rewrites (category)
  "Return a list of the possible rewrites for this caegory."
  (rule-rhs (assoc category *grammar*)))

(defun generate (phrase)
  "Generates a random sentence or phrase for *grammar*"
  (if (listp phrase)
      (mappend #'generate phrase)
      (let ((choices (rewrites phrase)))
        (if (null choices)
            (list phrase)
            (generate (random-elt choices))))))

(defparameter *bigger-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (article adj* noun pp*) (name) (pronoun))
    (verb-phrase -> (verb noun-phrase pp*))
    (pp* -> () (pp pp*))
    (adj* -> () (adj adj*))
    (pp -> (prep noun-phrase))
    (prep -> to in by with on)
    (adj -> big little blue green adiabatic)
    (article -> the a)
    (name -> pat kim lee terry robin)
    (noun -> man ball woman table)
    (verb -> hit took saw liked)
    (pronoun -> he she it these those that)))

(setf *grammar* *bigger-grammar*)

(defun generate-tree (phrase)
  (if (listp phrase)
      (mapcar #'generate-tree phrase)
      (let ((choices (rewrites phrase)))
        (if (null choices)
            (list phrase)
            (cons phrase
                  (generate-tree (random-elt choices)))))))

(defun generate-all (phrase)
  (cond ((null phrase) (list nil))
        ((listp phrase)
         (combine-all (generate-all (first phrase))
                      (generate-all (rest phrase))))
        ((rewrites phrase)
         (mappend #'generate-all (rewrites phrase)))
        (t (list (list phrase)))))

(defun cross-product (f a b)
  "Given binop F and lists A and B
generates a cross product of A and B
such as ((F A1 B1) (F A1 B2) .. (F AN B1) .. (F AN BN))"
  (mappend (lambda (y)
             (mapcar (lambda (x) (funcall f x y)) a))
           b))

(defun combine-all (xlist ylist)
  "(combine-all '((A) (B)) '((1) (2)))
  -> ((a 1) (b 1) (a 2) (b 2))"
  (cross-product #'append xlist ylist))

(setf *grammar* *simple-grammar*)

(defmacro while (test &body body)
  `(tagbody
    repeat
      (unless ,test
        (go end))
      ,@body
      (go repeat)
    end))

(defun print-pairs (list)
  (etypecase list
    (cons
     (princ "(")
     (princ (first list))
     (princ " . ")
     (print-pairs (rest list))
     (princ ")"))
    (null
     (princ list))))

(defun length-reduce (list)
  (reduce (lambda (acc x) (1+ acc)) list :initial-value 0))


