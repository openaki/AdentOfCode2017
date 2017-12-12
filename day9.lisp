



(defparameter *prev-char* #\Space)

(defun day-9-combined (str)
  (setf start-ignoring 'nil)
  (setf depth 0)
  (setf ignore-count 0)
  (setf group-depth '())
  (loop for c in (coerce str 'list) 
        do (progn
             (cond
               ((char= #\! *prev-char*) (progn))
               ((char= #\! c) (progn))
               ((char= c #\>) (setf start-ignoring 'nil))
               (start-ignoring (incf ignore-count))
               ((char= c #\<) (setf start-ignoring T))
               ((char= c #\{) (incf depth))
               ((char= c #\}) (progn (push depth group-depth) (decf depth)))
               )
             (setf *prev-char*
                   (if (and (char= *prev-char* #\!)
                            (char= c #\!))
                       #\space
                       c)
                   )))
  (list (apply #'+ group-depth) ignore-count)
  )

(day-9-combined "<{o'i!a,<{i<a>,")

(day-9-combined  "{{<a!>},{<a!>},{<a!>},{<ab>}}")


(defparameter *file-s* (with-open-file (stream "./input9.txt")
  (loop for line = (read-line stream nil)
        while line
        collect line)
  ))
(day-9-combined  (car *file-s*))


