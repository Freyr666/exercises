;; *******************************************************
;;
;; Part II
;;
;; The General Problem Solver
;; *******************************************************

(defun find-all (el list &key (test #'equalp))
  (reverse (reduce (lambda (acc mem)
                     (if (funcall test el mem)
                         (cons mem acc)
                         acc))
                   list
                   :initial-value '())))

(defpackage :gps
  (:use :common-lisp
   :common-lisp-user))

(in-package :gps)

(defvar *ops* nil
  "A list of available operators.")

(defstruct op
  "An operation"
  (action nil)
  (preconds nil)
  (add-list nil)
  (del-list nil))

(defun action-p (x)
  (or (equal x '(start))
      (executing-p x)))

(defun run (state goals &optional (*ops* *ops*))
  "Runs a general problem solver: achieve all
goals using *ops*"
  (remove-if-not #'action-p (achieve-all (cons '(start) state) goals nil)))

(defun use (oplist)
  (length (setf *ops* oplist)))

(defun starts-with (list el)
  (and (consp list) (eql (first list) el)))

(defun executing-p (x)
  "Is X of the form: (executing ...)?"
  (starts-with x 'executing))

(defun convert-op (op)
  "Make OP conform to the (EXECUTING op) convention."
  (unless (some #'executing-p (op-add-list op))
    (push (list 'executing (op-action op))
          (op-add-list op)))
  op)

(defun op (action &key preconds add-list del-list)
  "Make a new operator that obeys the (EXECUTING op) convention."
  (convert-op
   (make-op :action action :preconds preconds
            :add-list add-list :del-list del-list)))

(defun member-equal (item list)
  (member item list :test #'equal))

(defun achieve (state goal goal-stack)
  "A goal is achieved if it already holds
or if there is an appropriate op for it that is applicable."
  (cond ((member-equal goal state) state)
        ((member-equal goal goal-stack) nil)
        (t (some (lambda (op) (apply-op state goal op goal-stack))
                 (find-all goal *ops* :test #'appropriate-p)))))

(defun achieve-all (state goals goal-stack)
  "Try to achieve each goal, then make sure they still hold."
  (let ((current-state state))
    (if (and (every (lambda (g)
                      (setf current-state
                            (achieve current-state g goal-stack)))
                    goals)
             (subsetp goals current-state :test #'equal))
        current-state)))

(defun appropriate-p (goal op)
  "An op is appropriate to a goal if it is in its add list."
  (member-equal goal (op-add-list op)))

(defun apply-op (state goal op goal-stack)
  "Print a message and update *state* if op is applicable."
  (let ((new-state (achieve-all state
                                (op-preconds op)
                                (cons goal goal-stack))))
    (unless (null new-state)
      (append (remove-if (lambda (x)
                           (member-equal x (op-del-list op)))
                         new-state)
              (op-add-list op)))))

(when (achieve-all (op-preconds op))
  (print (list 'executing (op-action op)))
  (setf *state* (set-difference *state* (op-del-list op)))
  (setf *state* (union *state* (op-add-list op)))
  t))

;; **********************************************************************
;;
;; Testing
;;
;; **********************************************************************

(defparameter *school-ops*
  (mapc #'convert-op
        (list
         (make-op :action 'drive-son-to-school
                  :preconds '(son-at-home car-works)
                  :add-list '(son-at-school)
                  :del-list '(son-at-home))
         (make-op :action 'shop-installs-battery
                  :preconds '(car-needs-battery shop-knows-problem shop-has-money)
                  :add-list '(car-works))
         (make-op :action 'tell-shop-problem
                  :preconds '(in-communication-with-shop)
                  :add-list '(shop-knows-problem))
         (make-op :action 'telephone-shop
                  :preconds '(know-phone-number)
                  :add-list '(in-communication-with-shop))
         (make-op :action 'look-up-number
                  :preconds '(have-phone-book)
                  :add-list '(know-phone-number))
         (make-op :action 'give-shop-money
                  :preconds '(have-money)
                  :add-list '(shop-has-money)
                  :del-list '(have-money))
         (make-op :action 'ask-phone-number
                  :preconds '(in-communication-with-shop)
                  :add-list '(know-phone-number)))))

(use *school-ops*)

(run '(son-at-home car-needs-battery have-money have-phone-book)
     '(son-at-school))

(run '(son-at-home car-needs-battery have-money)
     '(son-at-school))

(run '(son-at-home car-works)
     '(son-at-school))


;; Monkey and Bananas

(defparameter *banana-ops*
  (list
   (op 'climb-on-chair
       :preconds '(chair-at-middle-room at-middle-room on-floor)
       :add-list '(at-bananas on-chair)
       :del-list '(at-middle-room on-floor))
   (op 'push-chair-from-door-to-middle-room
       :preconds '(chair-at-door at-door)
       :add-list '(chair-at-middle-room at-middle-room)
       :del-list '(chair-at-door at-door))
   (op 'walk-from-door-to-middle-room
       :preconds '(at-door on-floor)
       :add-list '(at-middle-room)
       :del-list '(at-door))
   (op 'grasp-bananas
       :preconds '(at-bananas empty-handed)
       :add-list '(has-bananas)
       :del-list '(empty-handed))
   (op 'drop-ball
       :preconds '(has-ball)
       :add-list '(empty-handed)
       :del-list '(has-ball))
   (op 'eat-bananas
       :preconds '(has-bananas)
       :add-list '(empty-handed not-hungry)
       :del-list '(has-bananas hungry))))

(use *banana-ops*)

(run '(at-door on-floor has-ball hungry chair-at-door)
     '(not-hungry))

;; Maze path

(defun make-maze-op (here there)
  (op `(move from ,here to ,there)
      :preconds `((at ,here))
      :add-list `((at ,there))
      :del-list `((at ,here))))

(defun make-maze-ops (pair)
  (list (make-maze-op (first pair) (second pair))
        (make-maze-op (second pair) (first pair))))

(defparameter *maze-ops*
  (apply #'append
         (mapcar #'make-maze-ops
                 '((1 2) (2 3) (3 4) (4 9) (9 14) (9 8) (8 7) (7 12) (12 13)
                   (12 11) (11 6) (11 16) (16 17) (17 22) (21 22) (22 23)
                   (23 18) (23 24) (24 19) (19 20) (20 15) (15 10) (10 5) (20 25))))
  "Defines operations for a maze as follows:
 1   2   3   4|  5
___________   |
 6|  7   8   9| 10
  |    ____   |
11  12  13| 14| 15
   _______|___|
16  17| 18| 19  20
___   |   |
21  22  23  24| 25")

(use *maze-ops*)

(run '((at 1)) '((at 25)))

;; The Block World Domain

(defun move-ons (a b c)
  (if (eq b 'table)
      `((,a on ,c))
      `((,a on ,c) (space on ,b))))

(defun move-op (a b c)
  "Make an operator to move A from B to C"
  (op `(move ,a from ,b to ,c)
      :preconds `((space on ,a) (space on ,c) (,a on ,b))
      :add-list (move-ons a b c)
      :del-list (move-ons a c b)))

(defun make-block-ops (blocks)
  (let ((ops nil))
    (dolist (a blocks)
      (dolist (b blocks)
        (unless (equal a b)
          (dolist (c blocks)
            (unless (or (equal c a) (equal c b))
              (push (move-op a b c) ops)))
          (push (move-op a 'table b) ops)
          (push (move-op a b 'table) ops))))
    ops))

(use (make-block-ops '(a b)))

(run '((a on b) (b on table) (space on a) (space on table))
     '((b on a) (a on table)))

(in-package :common-lisp-user)

(defun gen-permuts (list)
  (let (result)
    (labels ((for-each (prev elements)
               (when elements
                 (let ((el   (first elements))
                       (rest (rest elements)))
                   (setf result
                         (append (mapcar (lambda (l) (cons el l))
                                         (gen-permuts (append prev rest)))
                                 result))
                   (for-each (cons el prev) rest)))))
      (cond ((null list) nil)
            ((= (length list) 1)
             (list list))
            (t
             (for-each '() list)
             result)))))  

;; **********************************************************************
;;
;; Eliza
;;
;; **********************************************************************

(defpackage :eliza
  (:use :common-lisp))

(in-package :eliza)

(defun simple-equal (x y)
  "Are x and y equal?"
  (if (or (atom x) (atom y))
      (eql x y)
      (and (simple-equal (first x) (first y))
           (simple-equal (rest x) (rest y)))))

(defun variable-p (x)
  "Is x a variable (a symbol beginning with '?')?"
  (and (symbolp x)
       (equal (char (symbol-name x) 0)
              #\?)))

(defconstant fail nil "Indicates pat-match failure")

(defconstant no-bindings '((t . t))
  "Indicates pat-match success, with no variables.")

(defun get-binding (var bindings)
  (assoc var bindings))

(defun binding-val (binding)
  (cdr binding))

(defun lookup (var bindings)
  (binding-val (get-binding var bindings)))

(defun extend-bindings (var val bindings)
  (cons (cons var val)
        (if (eq bindings no-bindings)
            nil
            bindings)))

(defun match-variable (var input bindings)
  "Does VAR match input? Uses (or updates) and return bindings."
  (let ((binding (get-binding var bindings)))
    (cond ((not binding) (extend-bindings var input bindings))
          ((equal input (binding-val binding)) bindings)
          (t fail))))

(defun starts-with (list el)
  (and (consp list) (eql (first list) el)))

(defun segment-pattern-p (pattern)
  "Is this a segment matching pattern: ((?* var) . pat)"
  (and (consp pattern)
       (starts-with (first pattern) '?*)))

(defun segment-match (pattern input bindings &optional (start 0))
  "Match the segment pattern ((?* var) . pat) against input"
  (let ((var (second (first pattern)))
        (pat (rest pattern)))
    (if (null pat)
        (match-variable var input bindings)
        (let ((pos (position (first pat) input
                             :start start :test #'equal)))
          (if (null pos)
              fail
              (let ((b2 (pat-match
                         pat (subseq input pos)
                         (match-variable var (subseq input 0 pos)
                                         bindings))))
                (if (eq b2 fail)
                    (segment-match pattern input bindings (1+ pos))
                    b2)))))))

(defun pat-match (pattern input &optional (bindings no-bindings))
  "Does pattern match input?"
  (cond ((eq bindings fail) fail)
        ((variable-p pattern)
         (match-variable pattern input bindings))
        ((eql pattern input) bindings)
        ((segment-pattern-p pattern)
         (segment-match pattern input bindings))
        ((and (consp pattern) (consp input))
         (pat-match (rest pattern) (rest input)
                    (pat-match (first pattern) (first input)
                               bindings)))
        (t fail)))

(defun rule-pattern (rule)
  (first rule))

(defun rule-responses (rule)
  (rest rule))

(defparameter *eliza-rules*
  '((((?* ?x) hello (?* ?y))
     (How do you do. Please state your problem.))
    (((?* ?x) I want (?* ?y))
     (What would it mean if you got ?y)
     (Why do you want ?y)
     (Suppose you got ?y soon))
    (((?* ?x) if (?* ?y))
     (Do you really think its likely that ?y)
     (Do you with that ?y)
     (What do you think about ?y)
     (Really-- if ?y))
    (((?* ?x) no (?* ?y))
     (Why not ?)
     (You are being a bit negative)
     (Are you saying no just to be negative ?))
    (((?* ?x) I was (?* ?y))
     (Were you really ?)
     (Perhaps I already knew you were ?y)
     (Why do you tell me you were ?y now ?))
    (((?* ?x) I feel (?* ?y))
     (Do you often feel ?y ?))
    (((?* ?x) I felt (?* ?Y))
     (What other feelings do you have ?))))

(defun random-elt (list)
  (let ((len (length list)))
    (elt list (random len))))

(defun switch-viewpoint (words)
  (sublis '((I . you) (you . I)
            (me . you) (am . are))
          words))

(defun mklist (x)
  (if (listp x) x (list x)))

(defun flatten (the-list)
  (apply #'append
         (mapcar #'mklist the-list)))

(defun use-eliza-rules (input)
  "Find some rule whith which to transform the input"
  (some (lambda (rule)
          (let ((result (pat-match (rule-pattern rule) input)))
            (when (not (eq result fail))
              (sublis (switch-viewpoint result)
                      (random-elt (rule-responses rule))))))
        *eliza-rules*))

(defun eliza ()
  "Respond to user input using pattern matching rules"
  (loop
    (print 'eliza>)
    (write (flatten (use-eliza-rules (read))) :pretty t)))
