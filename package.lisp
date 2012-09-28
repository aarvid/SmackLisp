
;;;; package.lisp

(defpackage #:smacklisp
  (:use #:cl #:alexandria #:named-readtables)
  (:export smack
           fresh-smack
           smack-string
           smack-string-values
           *smack-symbols*
           init-smack-interp
            ))

