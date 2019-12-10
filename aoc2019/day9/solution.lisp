(defconstant +input+ "./input")

(load "../intcode/intcode.lisp")

(use-package :aoc.intcode)

(with-open-file (stream +input+)
  (eval-intcode-buffered (parse-intcode stream) '(1)))

;; Part Two

(with-open-file (stream +input+)
  (eval-intcode-buffered (parse-intcode stream) '(2)))
