(defpackage :aoc.intcode
  (:use :common-lisp)
  (:export :eval-intcode
           :eval-intcode-buffered
           :eval-intcode-scheduled
           :parse-intcode
           :make-scheduler
           :queue-routine
           :enqueue-routine
           :make-chan
           :write-chan
           :read-chan
           :chan->list))

(in-package :aoc.intcode)

(defconstant +page-size+ 512)

(defclass machine ()
  ((memory   :type hash-table)
   (pc       :type integer)
   (base     :type integer)))

(defmethod initialize-instance :after ((m machine) &key code)
  (setf (slot-value m 'pc) 0)
  (setf (slot-value m 'base) 0)
  (setf (slot-value m 'memory) (make-hash-table))
  (multiple-value-bind (page-num rest)
      (truncate (length code) +page-size+)
    (dotimes (p (1+ page-num))
      (let* ((page (make-array (list +page-size+) :element-type 'integer))
             (sz   (if (= p page-num)
                       rest
                       +page-size+)))
        (dotimes (rel sz)
          (setf (aref page rel)
                (aref code (+ (* p +page-size+)
                              rel))))
        (setf (gethash p (slot-value m 'memory))
              page)))))

(defmethod update-pc ((m machine) (pc-offset integer))
  (setf (slot-value m 'pc)
        (+ (slot-value m 'pc)
           pc-offset)))

(defmethod set-pc ((m machine) (new-pc integer))
  (setf (slot-value m 'pc)
        new-pc))

(defmethod update-base ((m machine) (base-offset integer))
  (setf (slot-value m 'base)
        (+ (slot-value m 'base)
           base-offset)))

(defun get-memory-page (m page-num)
  (when (not (gethash page-num (slot-value m 'memory)))
    (setf (gethash page-num (slot-value m 'memory))
          (make-array (list +page-size+)
                      :element-type 'integer)))
  (gethash page-num (slot-value m 'memory)))

(defun get-pc (m)
  (slot-value m 'pc))

(defun get-base (m)
  (slot-value m 'base))

(defmethod read-immediate ((m machine) (addr integer))
  (multiple-value-bind (page-num offset)
      (truncate addr +page-size+)
    (let ((page (get-memory-page m page-num)))
      (aref page offset))))
      
(defmethod read-position ((m machine) (addr integer))
  (multiple-value-bind (page-num offset)
      (truncate addr +page-size+)
    (let* ((page (get-memory-page m page-num))
           (pos  (aref page offset)))
      (multiple-value-bind (page-num-imm offset-imm)
          (truncate pos +page-size+)
        ;; Small optimization in case we
        ;; are looking for the same page
        (if (= page-num-imm page-num)
            (aref page offset-imm) 
            (aref (get-memory-page m page-num-imm) offset-imm))))))

(defmethod read-relative ((m machine) (addr integer))
  (multiple-value-bind (page-num offset)
      (truncate addr +page-size+)
    (let* ((page (get-memory-page m page-num))
           (pos  (+ (slot-value m 'base)
                    (aref page offset))))
      (multiple-value-bind (page-num-imm offset-imm)
          (truncate pos +page-size+)
        ;; Small optimization in case we
        ;; are looking for the same page
        (if (= page-num-imm page-num)
            (aref page offset-imm) 
            (aref (get-memory-page m page-num-imm) offset-imm))))))

(defmethod write-immediate ((m machine) (addr integer) (v integer))
  (multiple-value-bind (page-num offset)
      (truncate addr +page-size+)
    (let ((page (get-memory-page m page-num)))
      (setf (aref page offset) v))))

(defmethod write-position ((m machine) (addr integer) (v integer))
  (multiple-value-bind (page-num offset)
      (truncate addr +page-size+)
    (let* ((page (get-memory-page m page-num))
           (pos  (aref page offset)))
      (multiple-value-bind (page-num-imm offset-imm)
          (truncate pos +page-size+)
        ;; Small optimization in case we
        ;; are looking for the same page
        (if (= page-num-imm page-num)
            (setf (aref page offset-imm)
                  v)
            (setf (aref (get-memory-page m page-num-imm) offset-imm)
                  v))))))

(defmethod write-relative ((m machine) (addr integer) (v integer))
  (multiple-value-bind (page-num offset)
      (truncate addr +page-size+)
    (let* ((page (get-memory-page m page-num))
           (pos  (+ (slot-value m 'base)
                    (aref page offset))))
      (multiple-value-bind (page-num-imm offset-imm)
          (truncate pos +page-size+)
        ;; Small optimization in case we
        ;; are looking for the same page
        (if (= page-num-imm page-num)
            (setf (aref page offset-imm)
                  v)
            (setf (aref (get-memory-page m page-num-imm) offset-imm)
                  v))))))

(defclass inst ()
  ((modes :type vector)))

(defmethod arg-mode ((i inst) (arg-id integer))
  (let ((id (1- arg-id))
        (pvec (slot-value i 'modes)))
    (if (>= id (length pvec))
        :pos
        (aref pvec id))))

(defmethod read-arg ((i inst) (m machine) (n integer))
  (let ((pc (get-pc m)))
    (case (arg-mode i n)
      ((:pos) (read-position m (+ pc n)))
      ((:imm) (read-immediate m (+ pc n)))
      ((:rel) (read-relative m (+ pc n))))))

(defmethod read-opcode ((m machine))
  (let ((pc (get-pc m)))
    (read-immediate m pc)))

(defmethod write-res ((i inst) (m machine) (n integer) (v integer))
  (let ((pc (get-pc m)))
    (case (arg-mode i n)
      ((:pos) (write-position m (+ pc n) v))
      ((:imm) (write-immediate m (+ pc n) v))
      ((:rel) (write-relative m (+ pc n) v)))))

(defclass halt (inst)
  ((modes :type vector :initform #())))

(defclass input (inst)
  ((modes :type vector :initarg :modes)))

(defclass output (inst)
  ((modes :type vector :initarg :modes)))

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

(defclass adjust-base (inst)
  ((modes :type vector :initarg :modes)))

(defgeneric run (inst machine))

(defmethod run ((i halt) (m machine))
  nil)

(define-condition input-condition (error)
  ((machine :initarg :machine :reader input-condition-pc)))

(defun provide-input (x)
  (invoke-restart 'provide-input x))

(defmethod run ((i input) (m machine))
  (let* ((val (restart-case (error 'input-condition
                                   :machine m)
                (provide-input (x) x))))
    (write-res i m 1 val)
    (update-pc m 2)
    t))

(define-condition output-condition (error)
  ((value :initarg :value :reader output-condition-value)
   (machine :initarg :machine :reader output-condition-pc)))

(defun receive-output ()
  (invoke-restart 'receive-output))

(defmethod run ((i output) (m machine))
  (let* ((val (read-arg i m 1)))
    (print (restart-case
               (error 'output-condition
                      :value   val
                      :machine m)
             (receive-output () nil)))
    (update-pc m 2)
    t))

(defmethod run ((i addinst) (m machine))
  (let ((arg-1 (read-arg i m 1))
        (arg-2 (read-arg i m 2)))
    (write-res i m 3 (+ arg-1 arg-2))
    (update-pc m 4)
    t))

(defmethod run ((i mulinst) (m machine))
  (let ((arg-1 (read-arg i m 1))
        (arg-2 (read-arg i m 2)))
    (write-res i m 3 (* arg-1 arg-2))
    (update-pc m 4)
    t))

(defmethod run ((i jump-if-true) (m machine))
  (let ((arg-1 (read-arg i m 1))
        (arg-2 (read-arg i m 2)))
    (if (not (zerop arg-1))
        (set-pc m arg-2)
        (update-pc m 3))
    t))

(defmethod run ((i jump-if-false) (m machine))
  (let ((arg-1 (read-arg i m 1))
        (arg-2 (read-arg i m 2)))
    (if (zerop arg-1)
        (set-pc m arg-2)
        (update-pc m 3))
    t))

(defmethod run ((i less-than) (m machine))
  (let ((arg-1 (read-arg i m 1))
        (arg-2 (read-arg i m 2)))
    (write-res i m 3 (if (< arg-1 arg-2) 1 0))
    (update-pc m 4)
    t))

(defmethod run ((i equals) (m machine))
  (let ((arg-1 (read-arg i m 1))
        (arg-2 (read-arg i m 2)))
    (write-res i m 3 (if (= arg-1 arg-2) 1 0))
    (update-pc m 4)
    t))

(defmethod run ((i adjust-base) (m machine))
  (let ((arg-1 (read-arg i m 1)))
    (update-base m arg-1)
    (update-pc m 2)
    t))
  
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
    (let ((pvec (map 'vector (lambda (c) (case c
                                           ((#\0) :pos)
                                           ((#\1) :imm)
                                           ((#\2) :rel)))
                     (reverse (format nil "~d" params)))))
      (case op
        ((99) (make-instance 'halt))
        ((1)  (make-instance 'addinst :modes pvec))
        ((2)  (make-instance 'mulinst :modes pvec))
        ((3)  (make-instance 'input :modes pvec))
        ((4)  (make-instance 'output :modes pvec))
        ((5)  (make-instance 'jump-if-true :modes pvec))
        ((6)  (make-instance 'jump-if-false :modes pvec))
        ((7)  (make-instance 'less-than :modes pvec))
        ((8)  (make-instance 'equals :modes pvec))
        ((9)  (make-instance 'adjust-base :modes pvec))
        (t    (error op "Wrong opcode"))))))

(defun evaluate (m)
  (labels ((rec ()
             (let* ((inst    (decode-opcode (read-opcode m)))
                    (running (run inst m)))
               (if (not running)
                   nil
                   (rec)))))
    (rec)))

(defun eval-intcode (code)
  (let ((machine (make-instance 'machine :code code)))
    (handler-bind ((input-condition (lambda (ignore)
                                      (format t "Please provide a number:~%")
                                      (provide-input (parse-integer (read-line)))))
                   (output-condition (lambda (e)
                                       (format t "Output: ~A~%" (output-condition-value e))
                                       (receive-output))))
      (evaluate machine))))

(defun eval-intcode-buffered (code input-data)
  (let ((output '())
        (input  input-data)
        (machine (make-instance 'machine :code code)))
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
      (evaluate machine)
      (reverse output))))

;; Scheduling-aware evaluator

(defstruct sched
  (data nil :type list))

;; A simple round-robin scheduler
(defun make-scheduler ()
  (make-sched))

(defun queue-routine (sched r)
  (setf (sched-data sched)
        (append (sched-data sched) (list r))))

(defun enqueue-routine (sched)
  (when (sched-data sched)
    (let ((res (car (sched-data sched))))
      (setf (sched-data sched)
            (cdr (sched-data sched)))
      res)))

(defstruct chan
  (buffer nil :type list))

(defun write-chan (chan v)
  (setf (chan-buffer chan)
        (append (chan-buffer chan) (list v))))

(defun read-chan (chan)
  (if (chan-buffer chan)
      (let ((res (car (chan-buffer chan))))
        (setf (chan-buffer chan)
              (cdr (chan-buffer chan)))
        res)
      (error "Empty channel")))

(defun chan->list (chan)
  (chan-buffer chan))

(defun eval-intcode-scheduled-local (machine out-channel &key (input nil))
  (let ((input-data  input))
    (handler-bind ((input-condition
                    (lambda (e)
                      (if input-data
                          (let ((i input-data))
                            (setf input-data nil)
                            (provide-input i))
                          (return-from eval-intcode-scheduled-local
                            (lambda (input)
                              (eval-intcode-scheduled-local machine
                                                            out-channel
                                                            :input input))))))
                   (output-condition
                    (lambda (e)
                      (write-chan out-channel (output-condition-value e))
                      (receive-output))))
        (evaluate machine)
        nil)))

(defun eval-intcode-scheduled (code out-channel)
  (let ((machine (make-instance 'machine :code code)))
    (eval-intcode-scheduled-local machine out-channel)))
