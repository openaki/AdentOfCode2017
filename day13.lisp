

(defparameter *test-string*
  "0: 3
1: 2
4: 4
6: 4
")

(defun day13-1 (str &optional (seed 0))
  (let* ((il (with-input-from-string (s str)
               (loop for line = (read-line s 'nil)
                     while line
                     collect (mapcar #'parse-integer
                                     (split-sequence:split-sequence #\space
                                                                    (cl-ppcre:regex-replace-all ":" line  " ")
                                                                    :remove-empty-subseqs t)))))
         (max-index (reduce #'max il :key #'car))
         (arr  (make-array (+ max-index 1) :initial-element 0))
         (current-clock seed)
         (caught ()))
    (loop for i in il
          do (setf (aref arr (car i)) (  cadr i)))
    ;(print arr)
    (loop for i from 0 to max-index 
          do (progn
               (unless (= 0 (aref arr i))
                 (when (= 0 (rem current-clock (* 2 (1- (aref arr i)))))
                   (push (list i (aref arr i)) caught)))
               (incf current-clock)
               ))
    (print (apply #'+ (loop for i in caught collect (apply #'* i))))
    ;caught
    (let ((cc 0)
          (found 'nil))
      (loop
        ;(print cc)
        (setf found 'nil)
        (let ((current-clock cc)) 
          (loop for i from 0 to max-index 
                do (progn
                     (unless (= 0 (aref arr i)) (when (= 0 (rem current-clock (* 2 (- (aref arr i) 1)))) (setf found t)))
                     (incf current-clock)
                     )))
        (unless found  (return cc))
        (incf cc))
      (print cc)
      )
    )
)

(day13-1 *test-string*)

(defparameter *test-string*
  "0: 3
1: 2
2: 6
4: 4
6: 4
8: 8
10: 9
12: 8
14: 5
16: 6
18: 8
20: 6
22: 12
24: 6
26: 12
28: 8
30: 8
32: 10
34: 12
36: 12
38: 8
40: 12
42: 12
44: 14
46: 12
48: 14
50: 12
52: 12
54: 12
56: 10
58: 14
60: 14
62: 14
64: 14
66: 17
68: 14
72: 14
76: 14
80: 14
82: 14
88: 18
92: 14
98: 18
")
