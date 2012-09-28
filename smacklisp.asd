;;;; smacklisp.asd

(asdf:defsystem #:smacklisp
  :serial t
  :depends-on (#:alexandria
               #:metabang-bind
               #:cl-ppcre
               #:trivial-timeout
               #:named-readtables)
  :components ((:file "package")
               (:file "utils")
               (:file "smacklisp")
               (:file "cl-functions")               
               (:file "macros")
               (:file "setf")
               (:file "macrodefs")
               (:file "setfdefs")               
               ))

