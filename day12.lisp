(in-package #:aoc)


(defparameter *test-string*
  "0 <-> 2
1 <-> 1
2 <-> 0, 3, 4
3 <-> 2, 4
4 <-> 2, 3, 6
5 <-> 6
6 <-> 4, 5")

(defun parse-line (s g)
  ;(print s)
  (arrow-macros:-<>> s
    (cl-ppcre:regex-replace-all "<->" arrow-macros:<> " ")
    (cl-ppcre:regex-replace-all "," arrow-macros:<> " ")
    (split-sequence:split-sequence #\space arrow-macros:<> :remove-empty-subseqs t)
    (mapcar #'parse-integer arrow-macros:<>)
    (setf (gethash (car arrow-macros:<>) g) (cdr arrow-macros:<>))
    )
  )


(defun day12-combined (part1p)
  (let* ((*graph* (make-hash-table :test 'equal)))
    (with-input-from-file (s "./input12.txt")
                                        ;(with-input-from-string (s *test-string*)
      (loop for line = (read-line s nil)
            while line
            do (parse-line line *graph*)))

    (let* ((*nodes-left* (alexandria:hash-table-keys *graph*))
           (*all-groups* '()))

      (loop
        (let* ((*starting* (if part1p
                               (list 0)
                               (list (car *nodes-left*))))
               (*nodes-seen* *starting*)
               (working-nodes *starting*)
               (next-nodes '()))
          (loop
            (progn
              (loop for w in working-nodes
                    do (let* ((next (gethash w *graph*))
                              (new (set-difference next *nodes-seen*))
                              (next-nodes-seen (union *nodes-seen* new)))
                         (setf *nodes-seen* next-nodes-seen)
                         (setf next-nodes (union next-nodes new))
                         ))
              (setf working-nodes next-nodes)
              (setf next-nodes '())
              (when (= 0 (length working-nodes)) (return *nodes-seen*))
              ))
          (when part1p (return (length *nodes-seen*)))
          (push *nodes-seen* *all-groups*) ;; solution for part 1
          (setf *nodes-left* (set-difference *nodes-left* *nodes-seen*))
          (when (= 0(length *nodes-left*)) (return (length *all-groups*)))
          )))))


; part 1 sol
(day12-combined  T)

; part-2 sol
(day12-combined nil)

