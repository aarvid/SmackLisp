;;;; smacklisp.asd

(asdf:defsystem #:smacklisp
  :serial t
  :depends-on (#:alexandria
               #:metabang-bind
               #:cl-ppcre
               #:trivial-timeout
               #:arvid-utils)
  :components ((:file "package")
               (:file "smacklisp")
               (:file "macros")
               (:file "setf")
               (:file "macrodefs")
               (:file "setfdefs")               
               ))

