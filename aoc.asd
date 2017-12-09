;;;; aoc.asd

(asdf:defsystem #:aoc
  :description "Describe aoc here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:drakma
               #:babel
               #:cl-csv
               #:yason
               #:url-rewrite
               #:split-sequence
               #:arrow-macros)
  :serial t
  :components ((:file "package")))

