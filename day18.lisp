
(defparameter *sample-input*
  "set a 1
add a 2
mul a a
mod a 5
snd a
set a 0
rcv a
jgz a -1
set a 1
jgz a -2
")

;; TODO: intern the operations
(defun get-vector-of-operations (input)
  (let ((l (with-input-from-string (l input)
            (loop for x = (read-line l 'nil)
                  while x
                  collect (split-sequence:split-sequence #\space x :remove-empty-subseqs t))))
        )
    (make-array (length l) :initial-contents l)
    ))

(get-vector-of-operations *sample-input*)

(defun get-integer-or-hash-value (v hm)
  (if (parse-integer v :junk-allowed t)
      (parse-integer v :junk-allowed t)
      (gethash v hm 0)
      ))

(defun day18-1 (input)
  (let ((v (get-vector-of-operations input))
        (pcr 0)
        (reg-map (make-hash-table :test #'equal))
        (snd-played 0)
        )
    (loop
      (destructuring-bind (op a &optional (b 'nil)) (aref v pcr)
        (cond
          ((string= op "snd") (setf snd-played (get-integer-or-hash-value a reg-map)))
          ((string= op "set")  (setf (gethash a reg-map) (get-integer-or-hash-value b reg-map)))
          ((string= op "add") (incf (gethash a reg-map) (get-integer-or-hash-value b reg-map)))
          ((string= op "mul") (setf (gethash a reg-map) (* (gethash a reg-map 0)(get-integer-or-hash-value b reg-map))))
          ((string= op "mod") (setf (gethash a reg-map) (rem (gethash a reg-map 0) (get-integer-or-hash-value b reg-map))))
          ((string= op "rcv") (unless (= 0 (get-integer-or-hash-value a reg-map))  (return (print snd-played))))
          ((string= op "jgz") (when (> (get-integer-or-hash-value a reg-map) 0) (incf pcr (- (get-integer-or-hash-value b reg-map) 1))))
          )
        )
      (incf pcr))))


(day18-1 *sample-input*)
(day18-1 *test-input*)

(defun day18-2 (input)
  (let ((v (get-vector-of-operations input))
        (reg-map-0 (make-hash-table :test #'equal))
        (state-map-0 '(:id 0 :pcr 0 :recv-queue () :send-queue () :snd-counter 0 :waiting nil))

        (reg-map-1 (make-hash-table :test #'equal))
        (state-map-1 '(:id 1 :pcr 0 :recv-queue () :send-queue () :snd-counter 0 :waiting nil))
        )
    (labels ((handle-instruction (ins state-map reg-map id)
               (destructuring-bind (op a &optional (b 'nil)) ins
                 (cond
                   ((string= op "snd") (progn (push (get-integer-or-hash-value a reg-map)
                                                    (getf state-map :send-queue))
                                              (incf (getf state-map :snd-counter))))
                   ((string= op "set")  (setf (gethash a reg-map) (get-integer-or-hash-value b reg-map)))
                   ((string= op "add") (incf (gethash a reg-map) (get-integer-or-hash-value b reg-map)))
                   ((string= op "mul") (setf (gethash a reg-map) (* (gethash a reg-map 0)(get-integer-or-hash-value b reg-map))))
                   ((string= op "mod") (setf (gethash a reg-map) (rem (gethash a reg-map 0) (get-integer-or-hash-value b reg-map))))
                   ((string= op "rcv") (if (= 0 (length (getf state-map :recv-queue)))
                                           (progn (decf (getf state-map :pcr))
                                                  (setf (getf state-map :waiting) T))
                                           (progn (setf (getf state-map :waiting) nil)
                                                  (setf (gethash a reg-map)
                                                        (pop (getf state-map :recv-queue ))))))
                   ((string= op "jgz") (when (> (get-integer-or-hash-value a reg-map) 0)
                                         (incf (getf state-map :pcr)
                                               (- (get-integer-or-hash-value b reg-map) 1))))
                   )
                 )))
      (setf (gethash "p" reg-map-0) 0)
      (setf (gethash "p" reg-map-1) 1)
      (loop
        (handle-instruction (aref v (getf state-map-0 :pcr)) state-map-0 reg-map-0 0)
        (handle-instruction (aref v (getf state-map-1 :pcr)) state-map-1 reg-map-1 1)
        (when (getf state-map-0 :waiting) (progn (print "DONE") (return (print (getf state-map-1 :snd-counter)))))

        (setf (getf state-map-0 :recv-queue) (nconc (getf state-map-0 :recv-queue) (getf state-map-1 :send-queue)))
        (setf (getf state-map-1 :recv-queue) (nconc (getf state-map-1 :recv-queue) (getf state-map-0 :send-queue)))
        (setf (getf state-map-0 :send-queue) '())
        (setf (getf state-map-1 :send-queue) '())
        (incf (getf state-map-0 :pcr))
        (incf (getf state-map-1 :pcr ))))))

(day18-2 *sample-input2*)
(day18-2 *test-input*)

(defparameter *sample-input2*
  "snd 1
snd 2
snd p
rcv a
rcv b
rcv c
rcv d")

(defparameter *test-input*
  "set i 31
set a 1
mul p 17
jgz p p
mul a 2
add i -1
jgz i -2
add a -1
set i 127
set p 952
mul p 8505
mod p a
mul p 129749
add p 12345
mod p a
set b p
mod b 10000
snd b
add i -1
jgz i -9
jgz a 3
rcv b
jgz b -1
set f 0
set i 126
rcv a
rcv b
set p a
mul p -1
add p b
jgz p 4
snd a
set a b
jgz 1 3
snd b
set f 1
add i -1
jgz i -11
snd a
jgz f -16
jgz a -19 ")

