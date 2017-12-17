

(defparameter *t* '())
(defun day17-1 (steps iteration value-after)
  (declare (integer steps iteration value-after))
  (let ((curr-pos (the integer 0))
        (arr '(0)))
    (loop for i from 1 to iteration 
          do (let ((arr-size  i))
               (declare (integer arr-size))
               (setf curr-pos (rem (+ curr-pos steps) arr-size))
               (push i (cdr (nthcdr curr-pos arr)))
               (incf curr-pos (the integer 1))
               )
          )
    (setf *t* arr)
    (print (cadr (member value-after *t*)))
    )
  )

(day17-1 324 2018 2017)
(day17-1 324 50000000 0) ;; too slow ... we might need to remove all consing
(defun day17-2 (steps iteration)
  (let (
        (curr-pos 0))
    (setf arr '(0))
    (loop for i from 1 to iteration 
          do (let ((arr-size  i))
               (setf curr-pos (rem (+ curr-pos steps) arr-size))
               (when (= 0 curr-pos)  (push i arr))
               (incf curr-pos)
               )
          )
    (print (car arr)))
  )

(day17-2 324 50000000)
