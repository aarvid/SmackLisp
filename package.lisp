
;;;; package.lisp
(in-package :cl-user)

(defpackage #:smacklisp
  (:use #:cl #:alexandria #:named-readtables)
  (:export #:smack
           #:fresh-smack
           #:smack-string
           #:smack-string-values
           #:*smack-symbols*
           #:init-smack-interp
           #:load-file
           #:load-string
           #:load-stream
            ))

