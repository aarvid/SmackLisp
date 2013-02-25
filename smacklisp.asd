;;;; smacklisp.asd

(asdf:defsystem #:smacklisp
  :serial t
  :description "A common-lisp interpreter to run inside common lisp"
  :author "Andy Peterson <andy.arvid@gmail.com>"
  :license "MIT"
  :depends-on (#:alexandria
               #:metabang-bind
               #:cl-ppcre
               #:trivial-timeout
               #:iterate
               #:named-readtables)
  :components ((:file "package")
               (:file "utils")
               (:file "smacklisp")
               (:file "cl-functions")               
               (:file "macros")
               (:file "setf")
               (:file "macrodefs")
               (:file "setfdefs")               
               ;;(:file "puny-clos")
               ))

