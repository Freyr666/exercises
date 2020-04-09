;;
;; Part II
;;
;; The General Problem Solver
;;
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
