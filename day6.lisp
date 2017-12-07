(in-package #:aoc)

(defun distribute (qty xs start)
  (loop for i from 0 to (- qty 1)
        do (incf (nth (mod (+ i start) (length xs)) xs))))

(defun do-iteration (xs)
  (let ((seen '())
        (maxn 0)
        (position-max 0))
    (loop
      (setf maxn (apply #'max xs))
      (setf position-max (position maxn xs))
      (when (find xs seen :test #'equal)
        (return (list (length seen) (+ 1 (position xs seen :test #'equal)) )))
      (push (copy-seq xs) seen)
      (setf (nth position-max xs) 0)
      (distribute maxn xs (+ 1 position-max)))))


(defparameter xs '(0 2 7 0))
(do-iteration xs)

(do-iteration *test-list*)
(defparameter *test-list* '(4	10	4	1	8	4	9	14	5	1	14	15	0	15	3	5))
