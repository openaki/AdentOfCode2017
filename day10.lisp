

(defun my-subseq (l start len)
  (let ((end (rem (+ start len ) (length l))))
  (if (< start end)
      (subseq l start end)
      (nconc (subseq l start) (subseq l 0 end)))))

(defun over-write-subseq (orig1 new1 start1 olen1)
  (setf orig orig1)
  (setf new new1)
  (setf start start1)
  (setf olen olen1)

  (let* ((len (rem olen (length orig)))
         (loop-end1 (if (< (+ start len) (length orig))
                        (1- (+ start len))
                        (1- (length orig))))
         (second-loop-end1 (if (< (+ start len) (length orig)) 
                               -1
                               (1- (rem (+ start len ) (length orig)))))
         )
    (setf loop-end loop-end1)
    (setf second-loop-end second-loop-end1)
    (loop for i from start to loop-end 
          do (progn
               ;(print (nth (- i start) new))
               (setf (nth i orig) (nth (- i start) new))))
    (loop for i from 0 to second-loop-end 
          do (progn
               ;(print (nth (+ i (- (length orig) start)) new)) 
               (setf (nth i orig) (nth (+ i (- (length orig) start)) new)))))
  )

(setf i 0)

(defparameter *test-nums* '(94 84 0 79 2 27 81 1 123 93 218 23 103 255 254 243)) (defparameter *l* (loop for i from 0 to 255 collect i)) 

(defparameter *test-sample* '(3 4 1 5))

(defun day-10-1 (l test-nums)
  (loop for i in test-nums
        do (progn
             ;(print l)
             ;(print test-nums)
            (over-write-subseq l (reverse (my-subseq l next-index i)) next-index i)
            (setf next-index (rem (+ skip-size i next-index) (length l)))
            (incf skip-size)
            )
        )
  (* (car l) (cadr l)))

(day-10-1 *l*  *test-nums*)

(defparameter *test-string* "1,2,4")

(defun day-10-2 ()
  (let ((skip-size 0)
        (next-index 0)
        (*l-sample* (loop for i from 0 to 255 collect i))
        (*new-test* (nconc (map 'list #'char-code (coerce *test-string* 'list) ) '(17  31  73  47  23))))
    (loop for i from 0 to 63
          do (day-10-1 *l-sample*  *new-test*))
    (format nil "~{~a~}" (loop for i from 0 to 15
                               collect (string-downcase (format nil "~2,'0X" (apply #'logxor (subseq *l-sample* (* i 16) (* (1+ i) 16))) :base 16))))
    ))

(defparameter *test-string* "94,84,0,79,2,27,81,1,123,93,218,23,103,255,254,243")
(day-10-2)

