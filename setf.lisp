(in-package #:smacklisp)


(defmacro define-smack-setf (access-fn &rest rest)
  (cond ((and (car rest) (or (symbolp (car rest)) (functionp (car rest))))
         `(progn
            (set-smackprop ',access-fn 'setf-writer-fn ',(car rest))
            (rem-smackprop ',access-fn 'setf-lambda)
            (rem-smackprop ',access-fn 'setf-method)
            ',access-fn))
	(t
	 (let* ((args (first rest))
		(store (second rest))
		(body (cddr rest)))
	   `(progn
	      (set-smackprop ',access-fn 'setf-lambda
                             #'(lambda (,@store ,@args) ,@body))
	      (rem-smackprop ',access-fn 'setf-writer-fn)
	      (rem-smackprop ',access-fn 'setf-method)
	      ',access-fn)))))


;; lambda-blocks? see ecl code.
(defmacro define-smack-setf-expander (access-fn args &rest body)
  `(progn
     (set-smackprop ',access-fn
                    'SETF-METHOD
                    #'(lambda ,args ,@body))
     (rem-smackprop ',access-fn 'SETF-LAMBDA)
     (rem-smackprop ',access-fn 'SETF-writer-FN)
     ',access-fn))

(defun smack-get-setf-expansion (form &aux f)
  "Args: (form)
Returns the 'five gangs' (see DEFINE-SETF-EXPANDER) for PLACE as five values.
Does not check if the third gang is a single-element list."
  (flet ((rename-arguments (vars &aux names values all-args)
	   (dolist (item vars)
	     (unless (or (typep item 'fixnum) (keywordp item))
	       (push item values)
	       (setq item (gensym))
	       (push item names))
	     (push item all-args))
	   (values (gensym) (nreverse names) (nreverse values) (nreverse all-args))))
    ;; Note that macroexpansion of SETF arguments can only be done via
    ;; MACROEXPAND-1 [ANSI 5.1.2.7]
    (cond ((symbolp form)
	   (if (and (setq f (smack-macro-expand form)) (not (equal f form)))
	       (smack-get-setf-expansion f)
	       (let ((store (gensym)))
		 (values nil nil (list store) `(setq ,form ,store) form))))
	  ((or (not (consp form)) (not (symbolp (car form))))
	   (error "Cannot get the setf-method of ~S." form))
	  ((setq f (get-smackprop (car form) 'setf-method))
	   (apply f (cdr form)))
	  (t
	   (let* ((name (car form))
                  writer
                  (writer-fn (get-smackprop name 'setf-writer-fn))
                  (lambda-fn (unless writer-fn (get-smackprop (car form) 'setf-lambda))))
             (if (and (not writer-fn)
                      (not lambda-fn)
                      (setq f (smack-macro-expand form))
                      (not (equal f form)))
                 (smack-get-setf-expansion f)
                 (multiple-value-bind (store vars inits all)
                     (rename-arguments (cdr form))
                   (setq writer
                         (cond (writer-fn `(,writer-fn ,@all ,store))
                               (lambda-fn (apply lambda-fn store all))
                               (t `(funcall #'(setf ,name) ,store ,@all))))
                   (values vars inits (list store) writer (cons name all)))))))))


(defun setf-expand-1 (place newvalue)
  (multiple-value-bind (vars vals stores store-form access-form)
      (smack-get-setf-expansion place)
    (cond ((and (null vars) (null vals)
                (eq access-form place)
                (= (length stores) 1)
                (listp store-form)
                (= (length store-form) 3)
                (eq (first store-form) 'setq)
                (eq (second store-form) place)
                (eq (third store-form) (first stores)))
           (list 'setq place newvalue))
          ((and (consp place)
                (let* ((name (first place))
                       (inverse (get-smackprop name 'setf-writer-fn)))
                  (and inverse
                       (consp store-form)
                       (eq inverse (first store-form))
                       `(,inverse ,@(rest place) ,newvalue)))))
          (t `(let* ,(mapcar #'list vars vals)
                (multiple-value-bind ,stores ,newvalue
                  ,store-form))))))

(defun setf-expand (l)
  (cond ((endp l) nil)
        ((endp (cdr l)) (error "~S is an illegal SETF form." l))
        (t
         (cons (setf-expand-1 (car l) (cadr l))
               (setf-expand (cddr l) )))))

;; (def-smack-macro setf (&rest rest)
;;   (when (oddp (length rest))
;;     (error "odd number of args to SETf:~% ~S" rest))
;;   (cond ((endp rest) nil)
;;         ((endp (cddr rest))
;;          (setf-expand-1 (car rest) (cadr rest)))
;;         (t (cons 'progn (setf-expand rest)))))



