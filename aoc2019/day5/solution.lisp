(defconstant +input-path+ "./input")

(load "../intcode/intcode.lisp")

(use-package :aoc.intcode)

(with-open-file (stream +input-path+)
  (read-and-eval-intcode stream))
