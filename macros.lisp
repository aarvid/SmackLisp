(in-package #:smacklisp)

(defun smack-macro (symbol)
  (and (symbolp symbol)
       (get-smackprop symbol 'smack-macro)))

(defmacro def-smack-macro (name parmlist &body body)
  "Define a Smacklisp macro."
  `(set-smackprop ',name 'smack-macro #'(lambda ,parmlist .,body)))

(defun smack-macro-expand (x)
  "Macro-expand this Smacklisp expression."
  (if (and (listp x) (smack-macro (first x)))
      (smack-macro-expand
        (apply (smack-macro (first x)) (rest x)))
      x))

;;; ==============================

