
(defparameter *test-input* 368078)

(setf old-side (floor (sqrt *test-input*)))

(setf old-side (if (= 0 (mod old-side 2)) (1- old-side) old-side))

(setf side (+ old-side 2))

(setf diff (- *test-input* (* old-side old-side)))
;2053
;for each side we will put side - 1 numbers

(setf l (loop for i from 1 to 4 collect (* i (- side 1))))

(setf ans (apply #'min (remove-if-not (lambda (x) (> x 0)) (map 'list #'(lambda (x) (- x diff)) l))))


;note distance from center == distance from the corner for a element on the edge


;; PART 2

(defun sum-neigbour (tm x y)
  (+
   (aref tm (+ x 1) y)
   (aref tm (- x 1) y)
   (aref tm x (+ y 1) )
   (aref tm x (- y 1) )
   (aref tm (+ x 1) (+ y 1) )
   (aref tm (+ x 1) (- y 1) )
   (aref tm (- x 1) (+ y 1) )
   (aref tm (- x 1) (- y 1) )
   ))

(defun sum-neigbour-set (tm x y)
  (let ((s (sum-neigbour tm x y)))
    (setf (aref tm x y) s)
    (when (and (= *ans* 0) (> s *test-input*)) ; a really bad hack
        (setf *ans* s))))

(defun generate-next-square (tm n x y)
  (loop for i from 0 to (- n 2)
        do (sum-neigbour-set tm (- x i) (+ y 1)))
  (loop for i from 0 to (- n 2)
        do (sum-neigbour-set tm (- x (- n 2)) ( - y i)))
  (loop for i from 0 to (- n 2)
        do (sum-neigbour-set tm (+ (- x (- n 2) -1) i) (- y (- n 2))))
  (loop for i from 0 to (- n 2)
        do (sum-neigbour-set tm (+ x 1) (+ i (- y (- n 2) -1))))
  )

(defun day3-2 ()
  (defparameter *ans* 0)
  (setf tm (make-array '(101 101) :initial-element 0))
  (setf (aref tm 50 50) 1)
  (setf i 1)
  (loop
    (unless (= *ans* 0) (return *ans*))
    (generate-next-square tm (1+ (* 2  i)) (+ 49 i) (+ 49 i))
    (incf i))
  (list *ans* i)
  )

(day3-2)

