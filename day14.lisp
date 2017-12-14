

(defparameter *sample-key* "flqrgnkx")

(defparameter *input-key* "oundnydw")

(defun day-14-combined (key)
  (let* ((mem (loop for i from 0 to 127
                   collect (let ((row-key (format nil "~a-~a" key i)))
                             (coerce (format nil "~128,'0b" (parse-integer (day-10-2 row-key) :radix 16)) 'list)
                             )))
         (to-process (apply #'nconc (loop for i from 0 to 127
                                collect (loop for j from 0 to 127
                                              when (char= (nth j (nth i mem))  #\1)
                                                collect (list i j)))))
         (cnt 0)
         (queued '())
         (qc '())
         )
    (print (length (remove-if (lambda (x) (char= x #\0) ) (apply #'nconc mem))))
    (progn
      (setf cnt 0)
      (loop
        ;(print (length to-process))
        (when (= 0 (length to-process)) (return cnt))
        (setf queued '())
        (push (car to-process) queued)
        (loop
          (when (= 0 (length queued)) (return 'nil))
          (setf qc (car queued))
          (pop queued)
          (when (member qc to-process :test #'equal)
            (destructuring-bind (a b) qc
              (push (list (1+ a) b) queued)
              (push (list (1- a) b) queued)
              (push (list a (1+ b)) queued)
              (push (list a (1- b)) queued))
            (setf to-process (remove qc to-process :test #'equal))))
        (incf cnt))
      (print cnt))))

(day-14-combined *sample-key*)
(day-14-combined *input-key*)

