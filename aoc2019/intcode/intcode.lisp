(defpackage :aoc.intcode
  (:use :common-lisp)
  (:export :eval-intcode
           :eval-intcode-buffered 
           :parse-intcode
           :read-and-eval-intcode
           :read-and-eval-intcode-buffered))

(in-package :aoc.intcode)

(defclass inst ()
  ((modes :type vector)))

(defmethod direct-mode ((i inst) (id integer))
  (let ((id (1- id))
        (pvec (slot-value i 'modes)))
    (if (>= id (length pvec))
        nil
        (equal (aref pvec id) :imm))))

(defclass halt (inst)
  ((modes :type vector :initform #())))

(defclass input (inst)
  ((modes :type vector :initform #())))

(defclass output (inst)
  ((modes :type vector :initform #())))

(defclass addinst (inst)
  ((modes :type vector :initarg :modes)))

(defclass mulinst (inst)
  ((modes :type vector :initarg :modes)))

(defclass jump-if-true (inst)
  ((modes :type vector :initarg :modes)))

(defclass jump-if-false (inst)
  ((modes :type vector :initarg :modes)))

(defclass less-than (inst)
  ((modes :type vector :initarg :modes)))

(defclass equals (inst)
  ((modes :type vector :initarg :modes)))

(defgeneric run (inst pc mem))

(defmethod run ((i halt) (pc integer) (mem vector))
  nil)

(define-condition input-condition (error) ())

(defun provide-input (x)
  (invoke-restart 'provide-input x))

(defmethod run ((i input) (pc integer) (mem vector))
  (let ((val (restart-case (error 'input-condition)
               (provide-input (x) x)))
        (result-pos (elt mem (+ pc 1))))
    (setf (elt mem result-pos)
          val)
    (+ pc 2)))

(define-condition output-condition (error)
  ((value :initarg :value :reader output-condition-value)))

(defun receive-output ()
  (invoke-restart 'receive-output))

(defmethod run ((i output) (pc integer) (mem vector))
  (let ((result-pos (elt mem (+ pc 1))))
    (print (restart-case
               (error 'output-condition :value (elt mem result-pos))
             (receive-output () nil)))
    (+ pc 2)))

(defmethod run ((i addinst) (pc integer) (mem vector))
  (let ((arg-1 (if (direct-mode i 1)
                   (elt mem (+ pc 1))
                   (elt mem (elt mem (+ pc 1)))))
        (arg-2 (if (direct-mode i 2)
                   (elt mem (+ pc 2))
                   (elt mem (elt mem (+ pc 2)))))
        (result-pos (elt mem (+ pc 3))))
    (setf (elt mem result-pos)
          (+ arg-1 arg-2))
    (+ pc 4)))

(defmethod run ((i mulinst) (pc integer) (mem vector))
  (let ((arg-1 (if (direct-mode i 1)
                   (elt mem (+ pc 1))
                   (elt mem (elt mem (+ pc 1)))))
        (arg-2 (if (direct-mode i 2)
                   (elt mem (+ pc 2))
                   (elt mem (elt mem (+ pc 2)))))
        (result-pos (elt mem (+ pc 3))))
    (setf (elt mem result-pos)
          (* arg-1 arg-2))
    (+ pc 4)))

(defmethod run ((i jump-if-true) (pc integer) (mem vector))
  (let ((arg-1 (if (direct-mode i 1)
                   (elt mem (+ pc 1))
                   (elt mem (elt mem (+ pc 1)))))
        (arg-2 (if (direct-mode i 2)
                   (elt mem (+ pc 2))
                   (elt mem (elt mem (+ pc 2))))))
    (if (not (zerop arg-1))
        arg-2
        (+ pc 3))))

(defmethod run ((i jump-if-false) (pc integer) (mem vector))
  (let ((arg-1 (if (direct-mode i 1)
                   (elt mem (+ pc 1))
                   (elt mem (elt mem (+ pc 1)))))
        (arg-2 (if (direct-mode i 2)
                   (elt mem (+ pc 2))
                   (elt mem (elt mem (+ pc 2))))))
    (if (zerop arg-1)
        arg-2
        (+ pc 3))))

(defmethod run ((i less-than) (pc integer) (mem vector))
  (let ((arg-1 (if (direct-mode i 1)
                   (elt mem (+ pc 1))
                   (elt mem (elt mem (+ pc 1)))))
        (arg-2 (if (direct-mode i 2)
                   (elt mem (+ pc 2))
                   (elt mem (elt mem (+ pc 2)))))
        (result-pos (elt mem (+ pc 3))))
    (setf (elt mem result-pos)
          (if (< arg-1 arg-2) 1 0))
    (+ pc 4)))

(defmethod run ((i equals) (pc integer) (mem vector))
  (let ((arg-1 (if (direct-mode i 1)
                   (elt mem (+ pc 1))
                   (elt mem (elt mem (+ pc 1)))))
        (arg-2 (if (direct-mode i 2)
                   (elt mem (+ pc 2))
                   (elt mem (elt mem (+ pc 2)))))
        (result-pos (elt mem (+ pc 3))))
    (setf (elt mem result-pos)
          (if (= arg-1 arg-2) 1 0))
    (+ pc 4)))
  
;;
;; Parser
;;

(defun parse-char (char)
  (cond ((null char) nil)
        ((char= char #\,) :comma)
        ((char= char #\-) :neg)
        ((and (char>= char #\0)
              (char<= char #\9))
         (- (char-int char)
            (char-int #\0)))
        (t nil)))

(defun parse-intcode (stream)
  (labels ((rec (sign acc num-acc)
             (let ((symb (parse-char (read-char stream nil))))
               (cond
                 ((equal symb :comma)
                  (rec 1 (cons (* sign num-acc) acc) 0))
                 ((equal symb :neg)
                  (rec -1 acc num-acc))
                 ((numberp symb)
                  (rec sign acc (+ (* num-acc 10)
                                   symb)))
                 (t (cons num-acc acc))))))
    (apply #'vector (reverse (rec 1 '() 0)))))

(defun decode-opcode (code)
  (multiple-value-bind
        (params op) (truncate code 100)
    (let ((pvec (map 'vector (lambda (c) (if (char= c #\0) :pos :imm))
                     (reverse (format nil "~d" params)))))
      (case op
        ((99) (make-instance 'halt))
        ((1)  (make-instance 'addinst :modes pvec))
        ((2)  (make-instance 'mulinst :modes pvec))
        ((3)  (make-instance 'input))
        ((4)  (make-instance 'output))
        ((5)  (make-instance 'jump-if-true :modes pvec))
        ((6)  (make-instance 'jump-if-false :modes pvec))
        ((7)  (make-instance 'less-than :modes pvec))
        ((8)  (make-instance 'equals :modes pvec))
        (t    (error op "Wrong opcode"))))))

(defun eval-intcode (code)
  (handler-bind ((input-condition (lambda (ignore)
                                    (format t "Please provide a number:~%")
                                    (provide-input (parse-integer (read-line)))))
                 (output-condition (lambda (e)
                                     (format t "Output: ~A~%" (output-condition-value e))
                                     (receive-output))))
    (labels ((rec (pc)
               (let* ((inst   (decode-opcode (elt code pc)))
                      (new-pc (run inst pc code)))
                 (if (not new-pc)
                     code
                     (rec new-pc)))))
      (rec 0))))

(defun eval-intcode-buffered (code input-data)
  (let ((output '())
        (input  input-data))
    (handler-bind ((input-condition (lambda (ignore)
                                      (if input
                                          (let ((v (car input)))
                                            (setf input (cdr input))
                                            (provide-input v))
                                          (error "Input list is too short"))))
                   (output-condition (lambda (e)
                                       (setf output (cons (output-condition-value e)
                                                          output))
                                       (receive-output))))
      (labels ((rec (pc)
                 (let* ((inst   (decode-opcode (elt code pc)))
                        (new-pc (run inst pc code)))
                   (if (not new-pc)
                       code
                       (rec new-pc)))))
        (rec 0)
        (reverse output)))))

(defun read-and-eval-intcode (stream)
  "Reads and evaluates a program
written in Intcode"
  (let ((program (parse-intcode stream)))
    (eval-intcode program)))

(defun read-and-eval-intcode-buffered (stream input-data)
  "Reads and evaluates a program
written in Intcode"
  (let ((program (parse-intcode stream)))
    (eval-intcode-buffered program input-data)))
