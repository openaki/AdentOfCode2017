;;;; aoc.asd

(asdf:defsystem #:aoc
  :description "Describe aoc here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:drakma
               #:babel
               #:cl-csv
               #:yason
               #:url-rewrite)
  :serial t
  :components ((:file "package")
               (:file "aoc")))

