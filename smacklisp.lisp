;;; smacklisp.lisp

(in-package #:smacklisp)

;;; "smacklisp" goes here. Hacks and glory await!

(defun smack-lisp-implementation-type (denv)
  (declare (ignore denv))
  "SmackLisp")

(defun smack-lisp-implementation-version (denv)
  (declare (ignore denv))
  "0.0.01")


(defvar *smack-symbols*
  (make-hash-table ))

(defvar *max-interp-depth* 10000)

(defvar *smack-random-state*
  (make-random-state t))

(defun sharp-illegal (stream sub-char arg)
  (declare (ignore stream arg))
  (error  "illegal sharp macro character: ~S" sub-char))

(defreadtable smack-readtable
  (:merge :standard)
  (:dispatch-macro-char #\# #\. #'sharp-illegal)
  (:dispatch-macro-char #\# #\+ #'sharp-illegal)
  (:dispatch-macro-char #\# #\- #'sharp-illegal)
  (:dispatch-macro-char #\# #\= #'sharp-illegal)
  (:dispatch-macro-char #\# #\# #'sharp-illegal))



(defmacro with-smack-readtable (() &body body)
  `(with-named-readtable ('smack-readtable)
    ,@body))

(defmacro get-smackprop (symbol prop)
  `(getf (gethash ,symbol *smack-symbols*) ,prop))

(defmacro set-smackprop (symbol prop value)
  `(setf (get-smackprop ,symbol ,prop) ,value))

(defmacro rem-smackprop (symbol prop)
  `(remf (gethash ,symbol *smack-symbols*) ,prop))

(defmacro get-smackproperties (symbol props)
  `(get-properties (gethash ,symbol *smack-symbols*) ,props))

(defmacro smackprop-p (symbol prop)
  `(and (symbolp ,symbol)
        (get-smackproperties ,symbol (list ,prop))
        t))

(defmacro sl-read (&rest x)
  `(let ((*package* ,(find-package :smacklisp)))
     (read ,@x)))

(defmacro sl-read-from-string (&rest x)
  `(let ((*package* ,(find-package :smacklisp)))
     (read-from-string ,@x)))

(defun interp (x &optional env fenv denv)
  "Interpret (evaluate) the expression x in the variable environment env
   and function environment fenv"
  (cond
    ((keywordp x) x)
    ((symbolp x) (get-var x env denv))    
    ((atom x) x)
    ((smack-macro (first x))              
     (interp (smack-macro-expand x) env fenv denv))
    (t (case (first x)
         (quote    (second x))
         (progn    (smack-progn (rest x) env fenv denv))
         (setq     (smack-setq x env fenv denv))
         (if       (if (interp (second x) env fenv denv)
                       (interp (third x) env fenv denv)
                       (interp (fourth x) env fenv denv)))
         (lambda   (make-function
                    (second x) (cddr x) env fenv))
         (defun    (set-global-func ;; should be macro
                    (second x)
                    (make-function (third x) (cdddr x) env fenv)))
         (function (smack-function (second x) env fenv denv))
         (flet     (smack-flet (second x) (cddr x) env fenv denv))
         (labels   (smack-labels (second x) (cddr x) env fenv denv))
         (multiple-value-call
             (smack-multiple-value-call (second x) (cddr x) env fenv denv))
         (block    (smack-block (second x) (cddr x) env fenv denv))
         (return-from
          (smack-return-from (second x) (third x) env fenv denv))
         (tagbody  (smack-tagbody (rest x) env fenv denv))
         (go       (smack-go (rest x) env fenv denv))
          
         (t        (apply-function (first x)
                                   (interp-list (rest x) env fenv denv)
                                   env
                                   fenv
                                   denv))))))


(defun interp-toplevel (x &key timeout)
  (unless (smackprop-p t 'global-val)
    (init-smack-interp))
  (set-internal-var 'call-depth 0)
  (trivial-timeout:with-timeout (timeout)
    (let ((env (extend-env '(-) (list x) nil)))
      (let ((vals (multiple-value-list (interp x env nil nil))))
        (set-global-var '*** (get-global-var '**))
        (set-global-var '** (get-global-var '*))
        (set-global-var '* (car vals))
        (set-global-var '+++ (get-global-var '++))
        (set-global-var '++ (get-global-var '+))
        (set-global-var '+ x)
        (set-global-var '/// (get-global-var '//))
        (set-global-var '// (get-global-var '/))
        (set-global-var '/ vals)
        (values-list vals)))))



  

(defmacro smack-error-handle (&rest body)
  `(handler-case
       (progn ,@body)
     (simple-condition (condition)
       (apply 'format nil
              (simple-condition-format-control condition)
              (simple-condition-format-arguments condition)))
     (serious-condition (condition)
       (format nil "~A" condition))))
   

(defun smack-expr (x &key timeout)
  (smack-error-handle
   (interp-toplevel x :timeout timeout)))
 


(defun smack-string-simple (s &key timeout)
  "takes a string, reads it, interprets it and converts the result to a string"
  (with-output-to-string (ss)
    (with-smack-readtable ()
      (let* ((*standard-output* ss)
             (results (multiple-value-list
                       (interp-toplevel (sl-read-from-string s)
                                        :timeout timeout))))
        (mapcar #'fresh-print results)))))


(defun smack-string (s &key timeout)
  "takes a string, reads it, interprets it and converts the result to a string"
  (smack-error-handle
   (smack-string-simple s :timeout timeout)))

(defun smack-string-values (s &key timeout)
  "takes a string, reads it, interprets it and converts the result to a string"
  (let ((value-str (make-adjustable-string 0))
        (output-str (make-adjustable-string 0))
        (success-p nil))
    (with-output-to-string (sv value-str)
      (with-output-to-string (so output-str)
        (with-smack-readtable ()
          (handler-case
              (let* ((*standard-output* so)
                     (results (multiple-value-list
                               (interp-toplevel (sl-read-from-string s)
                                                :timeout timeout))))
                (let ((*standard-output* sv))
                  (mapcar #'fresh-print results))
                (setf success-p t))
            (simple-condition (condition)
              (apply 'format sv
                     (simple-condition-format-control condition)
                     (simple-condition-format-arguments condition)))
            (serious-condition (condition)
              (format sv "~A" condition))))))
    (values value-str output-str success-p)))

(defun smack (&key timeout)
  "A Smacklisp read-eval-print loop (using interp)"
  (with-smack-readtable ()
    (loop
      (clear-input )
      (format t "~&==> ")

      (when (eq :smack-quit
                (first (mapcar #'fresh-print
                               (multiple-value-list
                                (smack-error-handle
                                 (interp-toplevel (sl-read)
                                                  :timeout timeout))))))
        (return "Bye")))))


(defmacro fresh-smack (&rest x)
  `(let ((*smack-symbols* (make-hash-table)))
     (smack ,@x)))

(defun smack-tagbody (body env fenv denv)
  (let ((goto nil))
    (tagbody top
       (setq goto
             (catch 'tagbody-go
               (let ((evalp (not goto)))
                 (dolist (x body)
                   (if (symbolp x)
                       (setf evalp (or evalp (eql x goto)))
                       (when evalp
                         (interp x env fenv denv))))
                 nil)))
       (when goto
         (unless (member goto body)
           (throw 'tagbody-go goto))
         (go top)))))
          
(defun smack-go (body env fenv denv)
  (declare (ignore env fenv denv))
  (unless (singlep body)
    (error "Multiple arguments to GO"))
  (let ((tag (car body)))
    (unless (symbolp tag)
      (error "Argument to GO is not a symbol: ~a" tag))
    (throw 'tagbody-go tag)))




;; perhaps block environment?
(defun smack-block (name body env fenv denv)
  (let ((tag (gensym (string name))))
    (push tag (get-smackprop name 'block-tags))
    (unwind-protect 
         (catch tag (interp (cons 'progn body) env fenv denv))
      (pop (get-smackprop name 'block-tags)))))


(defun smack-return-from (name exp env fenv denv)
  (let ((tag (car (get-smackprop name 'block-tags))))
    (unless tag
      (error "unknown block tag: ~a " name))
      (throw tag (interp exp env fenv denv))))

(defun smack-multiple-value-call (function-form forms env fenv denv)
  (apply (interp function-form env fenv denv)
         denv
         (mapcan #'(lambda (x)
                     (multiple-value-list (interp x env fenv denv)))
                 forms)))


  
(defun smack-setq (x env fenv denv)
  (when (evenp (length x))
    (error "odd number of args to SETQ:~% ~S" x))
  (let ((result nil))
    (doplist (var val (cdr x) result)
      (setf result (set-var var (interp val env fenv denv) env denv)))))
  
  
(defun smack-function (arg env fenv denv)
  (declare (ignore denv))
  (cond ((symbolp arg)
         (get-func arg fenv))
        ((and (consp arg)
              (eq (first arg) 'lambda))
         (make-function (second arg)
                        (cddr arg)
                        env fenv))))

(defun smack-flet (fns body env fenv denv)
  (smack-progn body
               env
               (extend-env (mapcar #'first fns)
                           (mapcar #'(lambda (def)
                                       (make-function (second def)
                                                      (cddr def)
                                                      env
                                                      fenv))
                                   fns)
                           fenv)
               denv))

(defun smack-labels (fns body env fenv denv)
  (let ((new-fenv (extend-env (mapcar #'first fns)
                              (mapcar #'(lambda (def) def) fns)
                              fenv)))
    (mapc #'(lambda (def)
              (set-func (first def)
                        (make-function (second def)
                                       (cddr def)
                                       env
                                       new-fenv)
                        new-fenv))
          fns)
    (smack-progn body env new-fenv denv)))
                        
(defun force-keyword (S)
  "Make the symbol or string S into a keyword symbol."
  (unless (keywordp s)
    (etypecase S
      (string (intern (string-upcase S) :keyword))
      (symbol (intern (symbol-name S) :keyword)))))
  
(defun force-parm-keyword (parm)
  (if (consp parm)
      (rplaca parm (force-keyword (car parm)))
      (force-keyword parm)))

(defun need-symbol (x why)
  (unless (symbolp x)
    (error "~A is not a symbol: ~S" why x)))

(defun need-keyword (x why)
  (unless (keywordp x)
    (error "~A is not a keyword: ~S" why x)))

(defun smack-parse-lambda-list (lambda-list)
  (let ((required nil)
        (optional nil)
        (rest nil)
        (keyword nil)
        (state '&required))
    (dolist (p lambda-list)
      (case state
        (&required (if (member p '(&optional &key &rest))
                       (setq state p)
                       (progn (need-symbol p "Required argument" )
                              (push p required))))
        (&optional (cond ((member p '(&key &rest)) (setq state p))
                         ((eq p '&optional) (error "Two &optional keywords"))
                         (t (destructuring-bind (var &optional def sup-p)
                                (ensure-list p)
                              (need-symbol var "Optional argument" )
                              (need-symbol sup-p "Optional supplied-p argument" )
                              (push (list var def sup-p) optional)))))
        (&rest (cond ((eq p '&key) (setq state p))
                     ((eq p '&rest) (error "Two &rest keywords"))
                     ((eq p '&optional) (error "&optional keyword after &rest"))
                     ((consp rest) (error "Only one &rest parameter allowed"))
                     (t (need-symbol p "Rest argument" )
                        (push p rest))))
        (&key (cond ((eq p '&key) (error "Two &key keywords"))
                    ((eq p '&rest) (error "&rest keyword after &key"))
                    ((eq p '&optional) (error "&optional keyword after &key"))
                    (t (let (ky)
                         (destructuring-bind (var &optional def sup-p)
                                (ensure-list p)
                           (if (consp var)
                               (destructuring-bind (kyw vr) var
                                 (setq ky kyw var vr))
                               (setq ky (force-keyword var)))
                           (need-symbol var "Key argument" )
                           (need-symbol sup-p "Key supplied-p argument" )
                           (need-keyword ky  "Key argument" )
                           (push (list ky var def sup-p) keyword))))))))
    (values (reverse required) (reverse optional)
            (reverse rest) (reverse keyword))))




(defun make-function (parms code env fenv)
  (let ((bcode (maybe-add 'progn code)))
    (multiple-value-bind (required optional rest keyword)
        (smack-parse-lambda-list parms)
      #'(lambda (denv &rest args)
          (dolist (p required)
            (when (null args)
              (error "invalid number of arguments"))
            (if (special-var-p p denv)
                (setq denv (extend-env-1 p (pop args) denv))
                (setq env (extend-env-1 p (pop args) env))))
          (dolist (p optional)
            (destructuring-bind (var default supplied-p-var) p
              (let ((supplied-p (consp args))
                    (val (cond ((consp args) (pop args))
                               (default (interp default env fenv))
                               (t nil))))
                (if (special-var-p var denv)
                    (setq denv (extend-env-1 var val denv))
                    (setq env (extend-env-1 var val env)))
                (when supplied-p-var
                  (setq env (extend-env-1 supplied-p-var supplied-p env))))))
          (when (and args (null rest) (null keyword))
            (error "invalid number of arguments"))
          (dolist (p rest)
            (setq env (extend-env-1 p args env)))
          (unless (evenp (length args))
            (error "Odd number of key arguments"))
          (when keyword
            (doplist (key val args)
              (unless (member key keyword :key #'car)
                (error "Unknown key arqument: ~A" key)))
            (dolist (p keyword)
              (destructuring-bind (ky var default supplied-p-var) p
                (let* ((supplied-p (and (get-properties args `(,ky))
                                        t))
                       (val (cond (supplied-p (getf args ky))
                                  (default (interp default env fenv))
                                  (t nil))))
                  (if (special-var-p var denv)
                      (setq denv (extend-env-1 var val denv))
                      (setq env (extend-env-1 var val env)))
                  (when supplied-p-var
                    (setq env (extend-env-1 supplied-p-var supplied-p env)))))))
          (interp bcode env fenv denv)))))

;; (defun make-function (parms code env fenv)
;;   (let ((bcode (maybe-add 'progn code)))
;;     #'(lambda (&rest args)
;;         (interp bcode (extend-env parms args env) fenv))))
  

(defun apply-function (fn args env fenv denv)
  (let ((func (cond ((symbolp fn)
                     (get-func fn fenv))
                    ((and (consp fn) (eq (first fn) 'lambda))
                     (interp fn env fenv denv))
                    (t (error "Illegal function call: (~a ...)" fn))))
        (depth (get-internal-var 'call-depth))
        (max-depth (get-internal-var 'max-call-depth)))
    (when (>= depth max-depth)
      (error "Stack Overflow in Call to Function: (~a ...)" fn))
    (set-internal-var 'call-depth (1+ depth))
    (unwind-protect 
         (apply func denv args)
      (set-internal-var 'call-depth depth))))

(defun smack-apply (denv func arg &rest arguments)
  (let ((depth (get-internal-var 'call-depth))
        (max-depth (get-internal-var 'max-call-depth)))
    (when (>= depth max-depth)
      (error "Stack Overflow in Call to anonymous Function"))
    (set-internal-var 'call-depth (1+ depth))
    (unwind-protect 
         (cond ((atom arguments)
                (apply func denv arg))
               ((atom (cdr arguments))
                (apply func denv (cons arg (car arguments))))
               (t (do* ((a1 arguments a2)
                        (a2 (cdr arguments) (cdr a2)))
                       ((atom (cdr a2))
                        (rplacd a1 (car a2))
                        (apply func denv (cons arg arguments))))))
      (set-internal-var 'call-depth depth))))


;; returns a list of primary return values
(defun interp-list (exps env fenv denv)
  (mapcar #'(lambda (v) (interp v env fenv denv)) exps))
  
(defun smack-progn (exps env fenv denv)
  (let ((result nil))
    (dolist (x exps (values-list result))
      (setq result (multiple-value-list (interp x env fenv denv))))))

 
(defun extend-env (vars vals env)
  "Add some variables and values to an environment."
  (nconc (mapcar #'list vars vals) env))

(defun extend-env-1 (var val env)
  "Add one variable and value to an environment."
  (push (list var val) env))

(defun set-var (var val env denv)
  "Set a variable to a value, in the given, dynamic or global environment."
  (if (assoc var env)
      (setf (second (assoc var env)) val)
      (if (assoc var denv)
          (setf (second (assoc var denv)) val)
          (set-global-var var val)))
  val)
                     
                 
(defun get-var (var env denv)
  "Get the value of a variable, from the given, dynamic or global environment."
    (if (assoc var env)
        (second (assoc var env))
        (if (assoc var denv)
            (second (assoc var denv))
            (get-global-var var))))

(defun special-var-p (var denv)
  (or (assoc var denv)
      (smackprop-p var 'global-val)))

(defun set-global-var (var val)
  (when (get-smackprop var 'constant)
      (error "~a is a constant and can not be set." var))
  (set-smackprop var 'global-val val))

(defun set-internal-var (var val)
  (set-smackprop var 'internal-val val))



(defun get-global-var (var)
  (unless (smackprop-p var 'global-val) 
    (error "Unbound smacklisp variable: ~a" var))
  (get-smackprop var 'global-val))

(defun get-internal-var (var)
  (get-smackprop var 'internal-val))

(defun set-func (var val env)
  "Set a function name to a function, in the given or global environment."
  (if (assoc var env)
      (setf (second (assoc var env)) val)
      (set-global-func var val))
  val)
                     
                 
(defun get-func (symbol env)
  "Get the function of a function-name, from the given or global environment."
    (if (assoc symbol env)
        (second (assoc symbol env))
        (get-global-func symbol)))

(defun set-global-func (symbol val)
  (set-smackprop symbol 'global-func val))



(defun get-global-func (symbol)
  (unless (smack-fboundp nil symbol) 
    (error "Unbound smacklisp function: ~a" symbol))
  (get-smackprop symbol 'global-func))


(defun smack-quit (denv)
  (declare (ignore denv))
  :smack-quit)

(defun smack-constantp (denv form)
  (declare (ignore denv))
  (typecase form
      ;; copied from sbcl and altered
      (symbol
       (get-smackprop form 'constant))
      (list
       (eq (car form) 'quote))
      (t t)))

(defun smack-boundp (denv symbol)
  (declare (ignore denv))
  (smackprop-p symbol 'global-val))
      

(defun smack-fboundp (denv symbol)
  (declare (ignore denv))
  (smackprop-p symbol 'global-func))
      

(defun smack-fdefinition (denv symbol)
  (unless (smack-fboundp denv symbol)
    (error "Unbound global smacklisp function: ~a" symbol))
  (get-global-func symbol))

(defun smack-fmakunbound (denv symbol)
  (declare (ignore denv))
  (rem-smackprop symbol 'global-func))
  

(defun smack-funcall (denv fn &rest args)
  (smack-apply denv fn args))

(defun smack-defconstant (denv symbol val)
  (declare (ignore denv))
  (rem-smackprop symbol 'constant)
  (set-global-var symbol val)
  (set-smackprop symbol 'constant t)
  symbol)

  
(defun smack-random (denv limit)
  (declare (ignore denv))
  (random limit *smack-random-state*))

(defun smack-symbol-plist (denv symbol)
  (declare (ignore denv))
  (get-smackprop symbol 'symbol-plist))

(defun smack-get (denv symbol indicator &optional default)
  (declare (ignore denv))
  (getf (get-smackprop symbol 'symbol-plist) indicator default))

(defun smack-put-prop (denv symbol indicator value)
  (declare (ignore denv))
  (setf (getf (get-smackprop symbol 'symbol-plist) indicator)
        value))

(defun smack-putf (denv plist indicator value)
  (declare (ignore denv))
  (setf (getf plist indicator) value)
  plist)

(defun make-parms (slots) 
  (cons '&key
        (mapcar #'(lambda (s) (list (first s) (second s))) 
                slots)))
(defun make-body (internal-name slots)
  (list (apply #'append (list internal-name)
               (mapcar (lambda (s) (list (force-keyword (car s)) (car s)))
                       slots))))

(defun make-make-func (sname iname slots)
  (let ((smake (symbolicate 'make- sname))
        (imake (symbolicate 'make- iname)))
    (set-global-func
     smake
     (make-function (make-parms slots) (make-body imake slots) nil nil))
    (link-smack-cl-function imake imake)))

(defun  smack-defstruct (denv name options slots)
  (declare (ignore options denv))
  (let* ((simple-slots (mapcar #'car slots))
         (internal-name (gensym (string name))))
    (eval `(defstruct ,internal-name ,@simple-slots))
    (eval `(defmethod print-object ((str ,internal-name) stream)
             (write-string (cl-ppcre:regex-replace ,(string internal-name)
                                                   (with-output-to-string (s)
                                                     (call-next-method str s))
                                                   ,(string name))
                           stream)))
    (set-smackprop name 'structure internal-name)
    (make-make-func name internal-name slots)
    (link-smack-cl-function (symbolicate 'copy- name)
                            (symbolicate 'copy- internal-name))
    (link-smack-cl-function (symbolicate name '-p)
                            (symbolicate internal-name '-p))
    (dolist (slot simple-slots)
      (let ((s-slot (symbolicate name '- slot))
            (i-slot (symbolicate internal-name '- slot))
            (slot-setf (symbolicate internal-name '- slot '-setf)))
        (link-smack-cl-function s-slot i-slot)
        (set-global-func slot-setf
                         (eval `(lambda (y x) (setf (,i-slot x) y))))
        (set-smackprop s-slot ;; see define-smack-setf
                       'setf-lambda
                       #'(lambda (y x)
                           (list slot-setf y x)))))
    name))




(defun load-stream (stream &key print)
  (unless (smackprop-p t 'global-val)
    (init-smack-interp))
  (with-smack-readtable ()
    (do ((s (sl-read stream nil :eof nil)
            (sl-read stream nil :eof nil)))
        ((eq s :eof))
      (let ((result (interp s nil nil)))
        (when print
          (print result))))))

(defun load-string (string &key print)
  (with-input-from-string (stream string)
    (load-stream stream :print print)))

(defun load-file (file &key print)
  (with-input-from-file (stream file)
    (load-stream stream :print print)))

(defun smack-load-file (denv file &key print)
  (declare (ignore denv))
  (load-file file :print print))



(defun initialize-global-vars ()
  (set-global-var '*** nil)
  (set-global-var '** nil)
  (set-global-var '* nil)
  (set-global-var '+++ nil)
  (set-global-var '++ nil)
  (set-global-var '+ nil)
  (set-global-var '/// nil)
  (set-global-var '// nil)
  (set-global-var '/ nil)
  (set-global-var '- nil))

(defun initialize-internal-vars ()
  (set-internal-var 'call-depth 0)
  (set-internal-var 'max-call-depth (expt 2 10)))



(defun init-smack-constant (c)
  (if (listp c)
      (smack-defconstant nil (first c) (symbol-value (second c)))
      (smack-defconstant nil c (symbol-value c))))

(defun fresh-print (object &optional stream)
  (let ((stream (cond ((null stream) *standard-output*)
                      ((eq stream t) *terminal-io*)
                      (t stream))))
    (fresh-line stream)
    (prin1 object stream)
    (write-char #\space stream)
    object))





                            


  
(defun maybe-add (op exps &optional if-nil)
  "For example, (maybe-add 'and exps t) returns
  t if exps is nil, exps if there is only one,
  and (and exp1 exp2...) if there are several exps."
  (cond ((null exps) if-nil)
        ((singlep exps) (first exps))
        (t (cons op exps))))





       









#|(defun cxxr ()
  (let ((l nil))
    (do-all-symbols (x) 
      (when (and (fboundp x)
                 (eq (finda-package 'common-lisp)
                     (symbol-package x))
                 ((lambda (x)
                    (and (eq (first-elt x) #\C)
                         (eq (last-elt x) #\R)
                         (every #'(lambda (c) (or (eq c #\A) (eq c #\D)))
                                (subseq x 1 (1- (length x))))))
                  (string x)))
        (push x l)))
    (stable-sort (sort (remove-duplicates l) #'string< )
                 #'< :key (compose #'length #'string))))|#
  
  
