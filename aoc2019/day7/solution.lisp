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
      (let ((res 0))
        (block inf-loop
          (do ()
              (nil)
            (let ((res-lst (list res)))
              (dolist (v perm)
                (setf res-lst (eval-intcode-buffered (copy-seq code)
                                                     (cons v res-lst)))
                (if (null res-lst)
                    (return-from inf-loop)))
              (setf res (car res-lst)))))
        (when (> res max)
          (setf max res))))
    max))

;; Test 139629729
(with-input-from-string (stream "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5")
  (try-amplifiers-settings-recur stream))
