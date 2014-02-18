(in-package #:smacklisp)


(defun do-macro-help (do-macro vars test-result body)
  (let ((do-let (if (eq do-macro 'do*) 'let* 'let))
        (do-set (if (eq do-macro 'do*) 'setq 'psetq))
        (top-tag (gensym "top"))
        (var-init
         (mapcar #'(lambda (var-form)
                     (if (symbolp var-form)
                         var-form
                         (list (first var-form) (second var-form) )))
                 vars))
        (var-step nil))
    (dolist (var-form vars)
      (when (and (listp var-form) (third var-form))
        (push (first var-form) var-step)
        (push (third var-form) var-step)))
    `(block nil
       (,do-let ,var-init
         (tagbody ,top-tag
            (if ,(first test-result)
                (return ,(second test-result)))
            (progn ,@body)
            (,do-set ,@(reverse var-step))
            (go ,top-tag))))))

(defun sumf-help (op place delta)
  (multiple-value-bind (vars vals stores store-form access-form)
      (smack-get-setf-expansion place)
    (let ((d (gensym)))
      `(let* (,@(mapcar #'list vars vals)
              (,d ,delta)
              (,(car stores) (,op ,access-form ,d)))
         ,store-form))))




(defun initialize-system-macros ()
  (def-smack-macro let (bindings &rest body)
    `((lambda ,(mapcar #'first bindings) . ,body)
      .,(mapcar #'second bindings)))

  (def-smack-macro let* (bindings &rest body)
    (if (null bindings)
        `(progn .,body)
        `(let (,(first bindings))
           (let* ,(rest bindings) . ,body))))

  (def-smack-macro and (&rest args)
    (cond ((null args) 'T)
          ((singlep args) (first args))
          (t `(if ,(first args)
                  (and . ,(rest args))))))

  (def-smack-macro or (&rest args)
    (cond ((null args) 'nil)
          ((singlep args) (first args))
          (t (let ((var (gensym)))
               `(let ((,var ,(first args)))
                  (if ,var ,var (or . ,(rest args))))))))

  (def-smack-macro cond (&rest clauses)
    (cond ((null clauses) nil)
          ((singlep (first clauses))
           `(or ,(first (first clauses)) (cond .,(rest clauses))))
          ((starts-with (first clauses) 'else)
           `(progn .,(rest (first clauses))))
          (t `(if ,(first (first clauses))
                  (progn .,(rest (first clauses)))
                  (cond .,(rest clauses))))))

  (def-smack-macro case (key &rest clauses)
    (let ((key-val (gensym "KEY")))
      `(let ((,key-val ,key))
         (cond ,@(mapcar
                  #'(lambda (clause)
                      (if (or (starts-with 'otherwise clause)
                              (starts-with t clause))
                          `(t .,(rest clause))
                          `((member ,key-val ',(ensure-list (first clause)))
                            .,(rest clause))))
                  clauses)))))

  (def-smack-macro defvar (name &rest body)
    (if (atom name)
        `(progn
           (unless (boundp ',name)
             (setf ,name . ,body))
           ',name)
        nil))

  (def-smack-macro defparameter (name &rest body)
    (if (atom name)
        `(progn (setf ,name . ,body) ',name)
        nil))

  (def-smack-macro delay (computation)
    `(lambda () ,computation))

  (def-smack-macro defconstant (symbol val)
    `(%defconstant ',symbol ,val))

  (def-smack-macro setf (&rest rest)
    (when (oddp (length rest))
      (error "odd number of args to SETf:~% ~S" rest))
    (cond ((endp rest) nil)
          ((endp (cddr rest))
           (setf-expand-1 (car rest) (cadr rest)))
          (t (cons 'progn (setf-expand rest)))))

  (def-smack-macro when (exp &rest body)
    `(if ,exp
         (progn ,@body)))

  (def-smack-macro unless (exp &rest body)
    `(if (not ,exp)
         (progn ,@body)))

  (def-smack-macro psetq (&rest args)
    (let ((let-bindings nil)
          (setqs nil))
      (doplist (var val args)
        (let ((tmpvar (gensym)))
          (push (list tmpvar val) let-bindings)
          (push var setqs)
          (push tmpvar setqs)))
      `(let ,(reverse let-bindings)
         (setq ,@(reverse setqs))
         nil)))

  (def-smack-macro psetf (&rest args)
    (let ((let-bindings nil)
          (setfs nil))
      (doplist (var val args)
        (let ((tmpvar (gensym)))
          (push (list tmpvar val) let-bindings)
          (push var setfs)
          (push tmpvar setfs)))
      `(let ,(reverse let-bindings)
         (setf ,@(reverse setfs))
         nil))) 

  (def-smack-macro multiple-value-bind (vars form &rest body)
    `(multiple-value-call #'(lambda (,@vars ) ,@body) ,form))

  (def-smack-macro multiple-value-list (form)
    `(multiple-value-call 'list ,form))
  
  (def-smack-macro prog1 (first &rest body )
    (if (null body)
        first
        (let ((sym (gensym)))
          `(let ((,sym ,first))
             ,@body
             ,sym))))

  (def-smack-macro prog2 (first second &rest body)
    (let ((sym (gensym)))
      `(progn
         ,first
         (let ((,sym ,second))
           ,@body
           ,sym))))

  (def-smack-macro pop  (place)
    (multiple-value-bind (vars vals stores store-form access-form)
        (smack-get-setf-expansion place)
      (if (and (null vars) (eq access-form place))
          `(prog1 (car ,place) (setq ,place (cdr ,place)))
          `(let* ,(mapcar #'list
                   (append vars stores)
                   (append vals (list (list 'cdr access-form))))
             (prog1 (car ,access-form)
               ,store-form)))))
  
  (def-smack-macro push (item place)
    (multiple-value-bind (vars vals stores store-form access-form)
        (smack-get-setf-expansion place)
      (if (and (null vars) (eq access-form place))
          `(setq ,place (cons ,item ,place))
          (progn
            (unless (constantp item)
              (setq vals (cons item vals)
                    item (gensym)
                    vars (cons item vars)))
            `(let* ,(mapcar #'list
                     (append vars stores)
                     (append vals (list (list 'cons item access-form))))
               ,store-form)))))

  (def-smack-macro pushnew (item place &rest rest)
    (multiple-value-bind (vars vals stores store-form access-form)
        (smack-get-setf-expansion place)
      (if (and (null vars) (eq access-form place))
          `(setq ,place (adjoin ,item ,place ,@rest))
          (progn
            (unless (constantp item)
              (setq vals (cons item vals)
                    item (gensym)
                    vars (cons item vars)))
            `(let* ,(mapcar #'list
                     (append vars stores)
                     (append vals
                       (list (list* 'adjoin item access-form rest))))
               ,store-form)))))

  (def-smack-macro return (form)
    `(return-from nil ,form))
  
  (def-smack-macro do (vars test-result &rest body)
    (do-macro-help 'do vars test-result body))

  (def-smack-macro do* (vars test-result &rest body)
    (do-macro-help 'do* vars test-result body))
  
  (def-smack-macro dolist (var-list-result &rest body)
    (let ((var (first var-list-result))
          (lst (second var-list-result))
          (result (third var-list-result))
          (lvar (gensym)))
      (let ((vars `((,lvar ,lst (cdr ,lvar))
                    (,var (car ,lst) (cadr ,lvar))))
            (test-result
             `((null ,lvar) ,result)))
        (do-macro-help 'do vars test-result body))))

  (def-smack-macro dotimes (var-count-result &rest body)
    (let ((var (first var-count-result))
          (count (second var-count-result))
          (result (third var-count-result)))
      (let ((vars `((,var 0 (+ 1 ,var))))
            (test-result
             `((>= ,var ,count) ,result)))
        (do-macro-help 'do vars test-result body))))

  (def-smack-macro incf (place &optional (delta 1))
    (sumf-help '+ place delta))

  (def-smack-macro decf (place &optional (delta 1))
    (sumf-help '- place delta))
  (def-smack-macro defstruct (name-and-options &rest slot-descriptions)
    (destructuring-bind (name &rest options) (ensure-list name-and-options)
      (when options
        (error "Options not yet supported in defstruct"))
      (let ((slot-desc-normal (mapcar #'ensure-list slot-descriptions)))
        `(%defstruct ',name ',options ',slot-desc-normal))))
  (def-smack-macro prog (varlist &rest body) ;; no declarations
    `(block nil
       (let ,varlist
         (tagbody ,@body))))
  (def-smack-macro prog* (varlist &rest body)  ;; no declarations
    `(block nil
       (let* ,varlist
         (tagbody ,@body))))
  (def-smack-macro nth-value (n form) 
    `(nth ,n (multiple-value-list ,form)))
  t)









;; (define-smack-setf

