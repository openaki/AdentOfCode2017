

(defparameter *a-multiplier* 16807)
(defparameter *b-multiplier* 48271)

(defun day15-1 (a b)
  (let ((next-a a)
        (next-b b)
        (match-count 0))
    (dotimes (i 40000000)
      (setf next-a (rem (* next-a *a-multiplier*) 2147483647))
      (setf next-b (rem (* next-b *b-multiplier*) 2147483647))
      (when (= (logand next-a #xffff)
               (logand next-b #xffff))
        (incf match-count)))
    (print match-count)))


(day15-1 65 8921) ;; For sample

(day15-1 679 771) ;; For input


(defun day15-2 (a b)
  (let ((next-a a)
        (next-b b)
        (match-count 0)
        )
    (dotimes (i 5000000)
      (loop
        (setf next-a (rem (* next-a *a-multiplier*) 2147483647))
        (when (= 0 (mod next-a 4)) (return next-a)))
      (loop
        (setf next-b (rem (* next-b *b-multiplier*) 2147483647))
        (when (= 0 (mod next-b 8)) (return next-b)))
      (when (= (logand next-a #xffff)
               (logand next-b #xffff))
        (incf match-count)))
    (print match-count)))


(day15-2 65 8921) ;; For sample

(day15-2 679 771) ;; For input

