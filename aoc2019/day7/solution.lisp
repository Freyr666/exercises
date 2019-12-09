(defconstant +input+ "./input")

(load "../intcode/intcode.lisp")

(use-package :aoc.intcode)

(defun range (start end &optional (step 1))
  (loop for x from start below end by step collect x))

(defun gen-perms (start end)
  (labels ((recur (acc first rest)
             (if (null rest)
                 acc
                 (let ((el    (car rest))
                       (lower (recur '() '() (append first (cdr rest)))))
                   (if (null lower)
                       (recur (cons (list el) acc)
                              (cons el first)
                              (cdr rest))
                       (recur (append (mapcar (lambda (l) (cons el l)) lower)
                                      acc)
                              (cons el first)
                              (cdr rest)))))))
    (recur '() '() (range start end))))

(defun try-amplifiers-settings (stream)
  (let ((code   (parse-intcode stream))
        (max    0)
        (perms  (gen-perms 0 5)))
    (dolist (perm perms)
      (let ((res 0))
        (dolist (v perm)
          (setf res (car (eval-intcode-buffered (copy-seq code) (list v res)))))
        (when (> res max)
          (setf max res))))
    max))

;; Test 43210
(with-input-from-string (stream "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0")
  (when (not (= (try-amplifiers-settings stream) 43210))
    (error "Test failure")))

(with-open-file (stream +input+)
  (try-amplifiers-settings stream))

;; Part Two
(defun try-amplifiers-settings-recur (stream)
  (let ((code   (parse-intcode stream))
        (max    0)
        (perms  (gen-perms 5 10)))
    (dolist (perm perms)
      (let ((chan  (make-chan))
            (sched (make-scheduler)))
        (dolist (setting perm)
          (let ((kont (eval-intcode-scheduled (copy-seq code)
                                              chan
                                              :input setting)))
            (when kont
              (queue-routine sched kont))))
        ;; Write our initial value to channel
        (write-chan chan 0)
        ;; Main evaluation loop
        (block inf-loop
          (do ()
              (nil)
            (let ((routine (enqueue-routine sched)))
              (if (null routine)
                  (return-from inf-loop)
                  (let ((kont (funcall routine
                                       (read-chan chan))))
                    (when kont
                      (queue-routine sched kont)))))))
        (let ((res (apply #'max (chan->list chan))))
          (when (> res max)
            (setf max res)))))
    max))

;; Test 139629729
(with-input-from-string (stream "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5")
  (try-amplifiers-settings-recur stream))

(with-open-file (stream +input+)
  (try-amplifiers-settings-recur stream))
