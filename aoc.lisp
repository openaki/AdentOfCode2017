;;;; aoc.lisp

(in-package #:aoc)


;;; "aoc" goes here. Hacks and glory await!

(+ 1 1)


(concatenate 'string "abcd" "a")

(map nil #'princ "abcf")

(char-code #\0)

(parse-integer #\9)

(get-int #\9)
(loop
  for c across *my-string*
  collect (get-int c))

()

()

(subseq "abcd" 1)
(defun get-list-of-nums-space (str)
  (map 'list #'parse-integer (split-sequence:split-sequence #\space str :remove-empty-subseqs t)))


(single-row-checksum "5 1 9 5")


(apply #'min '(1 2 3))

(defun find-matching-pair-in-list (l)
  (let* ((f (first l))
         (sl (rest l))
         (ans (remove-if-not #'(lambda (x) (= (rem x f) 0) ) sl)))
    (if ans
        (cons f ans)
        'nil)))

(cons :a '(1 2))

(map ')
(rem 5 3)


(setf s '(2 3 4 5))

(remove-if-not #'(lambda (x) (= (car x) (cadr x))) '((1 1) (1 2)))

(find-divisble-pair-difference-in-list '(9 4 7 3))
(find-divisble-pair-difference-in-list '(3 8 6 5))

(defparameter *test-string*
  "5 9 2 8
9 4 7 3
3 8 6 5
")


tm

(setf (aref tm 2 1) 9)


(sum-neigbour  2 3)

(defun generate-next-square (n x y)
  (loop for i from 0 to (- n 2)
        do (sum-neigbour-set (- x i) (+ y 1)))
  (loop for i from 0 to (- n 2)
        do (sum-neigbour-set (- x (- n 2)) ( - y i)))
  (loop for i from 0 to (- n 2)
        do (sum-neigbour-set (+ (- x (- n 2) -1) i) (- y (- n 2))))
  (loop for i from 0 to (- n 2)
        do (sum-neigbour-set (+ x 1) (+ i (- y (- n 2) -1))))
  )

(loop for i from 1 to 3 by 1
      collect i)

(setf num 1)
(progn 
  (setf tm (make-array '(9 9) :initial-element 0))
  (setf (aref tm 3 3) 1)
  (generate-next-square 3 4 4)
  (generate-next-square 5 5 5)
  (generate-next-square 7 6 6)
  tm
  )

(setf tm (make-array '(10 10) :initial-element 0))
(coerce tm 'list)

(defun generate-sq (upto)
  (let* ((center (/ (+ 1 upto) 2)))
  (setf (aref tm  center center) 1)
  (loop for n from 0 to (/ (- upto 1) 2) by 1
        do (generate-next-square (+ 3 (* 2 n) ) (+ n center) (+ n center))
        ))
  )

(generate-sq 7)

num

(- 368078 (* 607 607)  )
(setf n 1921)

371

(- (- (- 371 (- n 1) (- n 1) (- n 1))

(s 368078)

(- 960 757)

(/ 1921 2)

(960)
(+ 203 960)

(/ 608 2) ; 304


(setf *ttt* "aa bb cc dd ee")

(setf x '(:a :b :c :a))


(pushnew :d x)

(list-length (reduce pushnew (split-sequence:split-sequence #\space *ttt*)))

(setf str "aa bb cc dd aa")

(defun duplicate-words-impl-p (str fn)
  (let ((original-list (split-sequence:split-sequence #\space str)))
    (if (= (list-length original-list) (list-length (remove-duplicates original-list :test fn)))
        1
        0)))

(defun duplicate-words-p (str)
  (duplicate-words-impl-p str #'equal))

(defparameter *test-string*
  "aad bb cc dd daa
bb cc dd
bb cc dd
bb cc dd
aaf gaa")

(sort "zygd" #'char-lessp)


(duplicate-words-p *test-string*)

(defun is-anagram-p (a b)
  (equal (sort a #'char-lessp)
         (sort b #'char-lessp)))

(defun duplicate-words-anagram-p (str)
  (duplicate-words-impl-p str #'is-anagram-p))

(is-anagram-p "abc" "ba")

(apply #'+ (map 'list #'duplicate-words-anagram-p (split-sequence:split-sequence #\linefeed *test-string* :remove-empty-subseqs t)) )

