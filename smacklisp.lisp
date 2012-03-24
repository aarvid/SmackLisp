;;; smacklisp.lisp

(in-package #:smacklisp)

;;; "smacklisp" goes here. Hacks and glory await!

(defun smack-lisp-implementation-type ()
  "SmackLisp")

(defun smack-lisp-implementation-version ()
  "0.0.01")


(defvar *smack-symbols*
  (make-hash-table ))

(defvar *max-interp-depth* 10000)

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

(defun interp (x &optional env fenv)
  "Interpret (evaluate) the expression x in the variable environment env
   and function environment fenv"
  (cond
    ((keywordp x) x)
    ((symbolp x) (get-var x env))    
    ((atom x) x)
    ((smack-macro (first x))              
     (interp (smack-macro-expand x) env fenv))
    (t (case (first x)
         (quote    (second x))
         (progn    (smack-progn (rest x) env fenv))
         (setq     (smack-setq x env fenv))
         (if       (if (interp (second x) env fenv)
                       (interp (third x) env fenv)
                       (interp (fourth x) env fenv)))
         (lambda   (make-function
                    (second x) (cddr x) env fenv))
         (defun    (set-global-func ;; should be macro
                    (second x)
                    (make-function (third x) (cdddr x) env fenv)))
         (function (smack-function (second x) env fenv))
         (flet     (smack-flet (second x) (cddr x) env fenv))
         (labels   (smack-labels (second x) (cddr x) env fenv))
         (multiple-value-call
             (smack-multiple-value-call (second x) (cddr x) env fenv))
         (block    (smack-block (second x) (cddr x) env fenv))
         (return-from
          (smack-return-from (second x) (third x) env fenv))
         (tagbody  (smack-tagbody (rest x) env fenv))
         (go       (smack-go (rest x) env fenv))
          
         (t        (apply-function (first x)
                                   (interp-list (rest x) env fenv)
                                   env
                                   fenv))))))


(defun interp-toplevel (x &key timeout)
  (unless (smackprop-p t 'global-val)
    (init-smack-interp))
  (set-internal-var 'call-depth 0)
  (trivial-timeout:with-timeout (timeout)
    (let ((env (extend-env '(-) (list x) nil)))
      (let ((vals (multiple-value-list (interp x env nil))))
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


(defun foo (s)
  (cond ((= s 1) 1) (t (foo (1- s)
                            ))))
  

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
 

(defun smack-string (s &key timeout)
  "takes a string, reads it, interprets it and converts the result to a string"
  (smack-error-handle
   (with-output-to-string (ss)
     (let* ((*standard-output* ss)
            (results (multiple-value-list
                         (interp-toplevel (read-from-string s)
                                          :timeout timeout))))
       (mapcar #'fresh-print results)))))

(defun smack (&key timeout)
  "A Smacklisp read-eval-print loop (using interp)"
  (loop
    (format t "~&==> ")

    (when (eq :smack-quit
              (first (mapcar #'fresh-print
                             (multiple-value-list
                                 (smack-error-handle
                                  (interp-toplevel (read)
                                                   :timeout timeout))))))
      (return "Bye"))))


(defmacro fresh-smack (&rest x)
  `(let ((*smack-symbols* (make-hash-table)))
     (smack ,@x)))

(defun smack-tagbody (body env fenv)
  (let ((goto nil))
    (tagbody top
       (setq goto
             (catch 'tagbody-go
               (let ((evalp (not goto)))
                 (dolist (x body)
                   (if (symbolp x)
                       (setf evalp (or evalp (eql x goto)))
                       (when evalp
                         (interp x env fenv))))
                 nil)))
       (when goto
         (unless (member goto body)
           (throw 'tagbody-go goto))
         (go top)))))
          
(defun smack-go (body env fenv)
  (declare (ignore env fenv))
  (unless (singlep body)
    (error "Multiple arguments to GO"))
  (let ((tag (car body)))
    (unless (symbolp tag)
      (error "Argument to GO is not a symbol: ~a" tag))
    (throw 'tagbody-go tag)))




;; perhaps block environment?
(defun smack-block (name body env fenv)
  (let ((tag (gensym (string name))))
    (push tag (get-smackprop name 'block-tags))
    (unwind-protect 
         (catch tag (interp (cons 'progn body) env fenv))
      (pop (get-smackprop name 'block-tags)))))


(defun smack-return-from (name exp env fenv)
  (let ((tag (car (get-smackprop name 'block-tags))))
    (unless tag
      (error "unknown block tag: ~a " name))
      (throw tag (interp exp env fenv))))

(defun smack-multiple-value-call (function-form forms env fenv)
  (apply (interp function-form env fenv)
         (mapcan #'(lambda (x)
                     (multiple-value-list (interp x env fenv)))
                 forms)))


  
(defun smack-setq (x env fenv)
  (when (evenp (length x))
    (error "odd number of args to SETQ:~% ~S" x))
  (let ((result nil))
    (doplist (var val (cdr x) result)
      (setf result (set-var var (interp val env fenv) env)))))
  
;;  (do* ((var (second x) (first y))
;;        (val (third x) (second y))
;;        (y  (cdddr x) (cddr y))
;;        (result (set-var var (interp val env fenv) env)
;;                (set-var var (interp val env fenv) env)))
;;       ((null y) result)))
;;(setq   (set-var (second x) (interp (third x) env fenv) env))
  
(defun smack-function (arg env fenv)
  (cond ((symbolp arg)
         (get-func arg fenv))
        ((and (consp arg)
              (eq (first arg) 'lambda))
         (make-function (second arg)
                        (cddr arg)
                        env fenv))))

(defun smack-flet (fns body env fenv)
  (smack-progn body
               env
               (extend-env (mapcar #'first fns)
                           (mapcar #'(lambda (def)
                                       (make-function (second def)
                                                      (cddr def)
                                                      env
                                                      fenv))
                                   fns)
                           fenv)))

(defun smack-labels (fns body env fenv)
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
    (smack-progn body env new-fenv)))
                        
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

(defun smack-parse-parms (parms)
  (let ((required nil)
        (optional nil)
        (rest nil)
        (keyword nil)
        (state '&required))
    (dolist (p parms)
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
        (smack-parse-parms parms)
      #'(lambda (&rest args)
          (dolist (p required)
            (when (null args)
              (error "invalid number of arguments"))
            (setq env (extend-env-1 p (pop args) env)))
          (dolist (p optional)
            (destructuring-bind (var default supplied-p-var) p
              (let ((supplied-p (consp args))
                    (val (cond ((consp args) (pop args))
                               (default (interp default env fenv))
                               (t nil))))
                (setq env (extend-env-1 var val env))
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
                  (setq env (extend-env-1 var val env))
                  (when supplied-p-var
                    (setq env (extend-env-1 supplied-p-var supplied-p env)))))))
          (interp bcode env fenv)))))

;; (defun make-function (parms code env fenv)
;;   (let ((bcode (maybe-add 'progn code)))
;;     #'(lambda (&rest args)
;;         (interp bcode (extend-env parms args env) fenv))))
  

(defun apply-function (fn args env fenv)
  (let ((func (cond ((symbolp fn)
                     (get-func fn fenv))
                    ((and (consp fn) (eq (first fn) 'lambda))
                     (interp fn env fenv))
                    (t (error "Illegal function call: (~a ...)" fn))))
        (depth (get-internal-var 'call-depth))
        (max-depth (get-internal-var 'max-call-depth)))
    (when (>= depth max-depth)
      (error "Stack Overflow in Call to Function: (~a ...)" fn))
    (set-internal-var 'call-depth (1+ depth))
    (unwind-protect 
         (apply func args)
      (set-internal-var 'call-depth depth))))

;; returns a list of primary return values
(defun interp-list (exps env fenv)
  (mapcar #'(lambda (v) (interp v env fenv)) exps))
  
(defun smack-progn (exps env fenv)
  (let ((result nil))
    (dolist (x exps (values-list result))
      (setq result (multiple-value-list (interp x env fenv))))))
;;  (values-list
;;   (lastcar (mapcar #'(lambda (v) (multiple-value-list (interp v env fenv)))
;;                    exps))))
 


(defun set-var (var val env)
  "Set a variable to a value, in the given or global environment."
  (if (assoc var env)
      (setf (second (assoc var env)) val)
      (set-global-var var val))
  val)
                     
                 
(defun get-var (var env)
  "Get the value of a variable, from the given or global environment."
    (if (assoc var env)
        (second (assoc var env))
        (get-global-var var)))

(defun set-global-var (var val)
  (if (smack-constantp var)
      (error "~a is a constant and can not be set." var))
  (set-smackprop var 'global-val val))

(defun set-internal-var (var val)
  (set-smackprop var 'internal-val val))

(defun smack-constantp (symbol)
  (get-smackprop symbol 'constant))

(defun get-internal-var (var)
  (get-smackprop var 'internal-val))

(defun set-func (var val env)
  "Set a variable to a value, in the given or global environment."
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
  (unless (smack-fboundp symbol) 
    (error "Unbound smacklisp function: ~a" symbol))
  (get-smackprop symbol 'global-func))


;;(defmacro defun-smack (parms &rest body)
;;  `  (set-global-func name
;;                   (lambda
(defun smack-quit()
  :smack-quit)

(defun smack-boundp (symbol)
  (smackprop-p symbol 'global-val))
      

(defun smack-fboundp (symbol)
  (smackprop-p symbol 'global-func))
      

(defun smack-fdefinition (symbol)
  (unless (smack-fboundp symbol)
    (error "Unbound global smacklisp function: ~a" symbol))
  (get-global-func symbol))

(defun smack-fmakunbound (symbol)
  (rem-smackprop symbol 'global-func))
  

(defun smack-funcall (fn &rest args)
  (apply fn args))

(defun smack-defconstant (symbol val)
  (rem-smackprop symbol 'constant)
  (set-global-var symbol val)
  (set-smackprop symbol 'constant t))

(defun smack-defun (symbol parms body env fenv)
  (set-global-func symbol (make-function parms body env fenv)))
  
(defun extend-env (vars vals env)
  "Add some variables and values to an environment."
  (nconc (mapcar #'list vars vals) env))

(defun extend-env-1 (var val env)
  "Add one variable and value to an environment."
  (push (list var val) env))

(defun smack-symbol-plist (symbol)
  (get-smackprop symbol 'symbol-plist))

(defun smack-get (symbol indicator &optional default)
 (getf (get-smackprop symbol 'symbol-plist) indicator default))

(defun smack-put-prop (symbol indicator value)
  (setf (getf (get-smackprop symbol 'symbol-plist) indicator)
        value))

(defun smack-putf (plist indicator value)
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

(defun  smack-defstruct (name options slots)
  (declare (ignore options))
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



(defparameter *smack-procs*
  '(
    ;; Evaluation and Compilation
    (constantp smack-constantp)

    ;; Control and data flow    
    eq equal eql
    not notany some every notevery 
    identity
    apply complement constantly
    functionp
    values values-list
    (fboundp smack-fboundp)
    (fdefinition smack-fdefinition)
    (fmakunbound smack-fmakunbound)
    (funcall smack-funcall)
    
    ;; structures
    copy-structure
    
    ;; symbols
    keywordp  ;; ??
    (boundp smack-boundp)

    ;; numbers
    + - * / = < > <= >= /=
    abs acos acosh ash asin asinh atan atanh
    boole byte ceiling cis complex complexp
    conjugate cos cosh decode-float
    denominator evenp exp expt fceiling ffloor float floatp
    float-sign floor fround ftruncate gcd imagpart integerp
    isqrt lcm log max min minusp mod numberp numerator oddp
    parse-integer phase plusp rational rationalize rationalp
    realp realpart rem round signum sin sinh sqrt tan tanh
    truncate zerop 1+ 1-

    ;; characters
    alpha-char-p both-case-p alphanumericp
    character characterp char-code char-downcase
    char-greaterp char-equal char-int char-lessp
    char-name char-not-greaterp char-not-equal
    char-not-lessp char-upcase char= char/=
    char> char< char>= char<= code-char
    digit-char graphic-char-p lower-case-p
    name-char standard-char-p upper-case-p

    ;; conses    
    cons append list member
    car cdr
    caar cadr cdar cddr
    caaar caadr cadar caddr cdaar cdadr cddar cdddr
    caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
    cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
    first second third fourth fifth sixth seventh eighth ninth tenth
    rest nthcdr nth last endp
    consp listp atom null
    copy-list copy-alist copy-tree
    list-length adjoin
    rplaca rplacd
    mapc mapcan mapcar mapcon mapl maplist
    assoc acons assoc-if
    getf get-properties
    (get smack-get)
    (symbol-plist smack-symbol-plist)
    (%sys-put-prop smack-put-prop)
    (%sys-putf smack-putf)
    
    ;; arrays
    adjustable-array-p adjust-array aref array-dimension array-dimensions
    array-displacement array-element-type array-has-fill-pointer-p
    array-in-bounds-p arrayp array-rank array-row-major-index
    array-total-size make-array row-major-aref simple-vector-p
    svref vector vectorp vector-pop vector-push vector-push-extend
    
    ;; strings
    char make-string schar simple-string-p string
    string-capitalize nstring-capitalize
    string-downcase nstring-downcase
    string-upcase nstring-upcase
    string-equal string-greaterp string-left-trim
    string-lessp string-not-equal string-not-greaterp
    string-not-lessp stringp string-right-trim
    string-trim string= string/= string< string>
    string<= string>=
    
    ;; sequences
    concatenate copy-seq count count-if count-if-not
    elt fill find find-if length make-sequence
    map map-into merge mismatch position position-if
    reduce remove delete
    remove-duplicates delete-duplicates
    remove-if delete-if
    replace reverse nreverse search sort stable-sort
    subseq substitute nsubstitute substitute-if nsubstitute-if
    
    ;; hash tables
    clrhash gethash hash-table-count hash-table-p
    hash-table-rehash-size hash-table-rehash-threshold
    hash-table-size hash-table-test
    make-hash-table maphash remhash sxhash
    
    ;; reader
    read
    ;; printer
    prin1 princ terpri print    
    ;; system construction
    (load load-file)
    ;; environment
    decode-universal-time encode-universal-time
    get-decoded-time get-internal-real-time
    get-universal-time
    (lisp-implementation-type smack-lisp-implementation-type)
    (lisp-implementation-version smack-lisp-implementation-version)
    ;; smacklisp extension
    (%defconstant smack-defconstant)
    (%defstruct smack-defstruct)
    (quit smack-quit)))


(defparameter *smack-constants*
  '(
    ARRAY-DIMENSION-LIMIT ARRAY-RANK-LIMIT ARRAY-TOTAL-SIZE-LIMIT BOOLE-1 BOOLE-2
    BOOLE-AND BOOLE-ANDC1 BOOLE-ANDC2 BOOLE-C1 BOOLE-C2 BOOLE-CLR BOOLE-EQV
    BOOLE-IOR BOOLE-NAND BOOLE-NOR BOOLE-ORC1 BOOLE-ORC2 BOOLE-SET BOOLE-XOR
    CALL-ARGUMENTS-LIMIT CHAR-CODE-LIMIT DOUBLE-FLOAT-EPSILON
    DOUBLE-FLOAT-NEGATIVE-EPSILON INTERNAL-TIME-UNITS-PER-SECOND
    LAMBDA-LIST-KEYWORDS LAMBDA-PARAMETERS-LIMIT LEAST-NEGATIVE-DOUBLE-FLOAT
    LEAST-NEGATIVE-LONG-FLOAT LEAST-NEGATIVE-NORMALIZED-DOUBLE-FLOAT
    LEAST-NEGATIVE-NORMALIZED-LONG-FLOAT LEAST-NEGATIVE-NORMALIZED-SHORT-FLOAT
    LEAST-NEGATIVE-NORMALIZED-SINGLE-FLOAT LEAST-NEGATIVE-SHORT-FLOAT
    LEAST-NEGATIVE-SINGLE-FLOAT LEAST-POSITIVE-DOUBLE-FLOAT
    LEAST-POSITIVE-LONG-FLOAT LEAST-POSITIVE-NORMALIZED-DOUBLE-FLOAT
    LEAST-POSITIVE-NORMALIZED-LONG-FLOAT LEAST-POSITIVE-NORMALIZED-SHORT-FLOAT
    LEAST-POSITIVE-NORMALIZED-SINGLE-FLOAT LEAST-POSITIVE-SHORT-FLOAT
    LEAST-POSITIVE-SINGLE-FLOAT LONG-FLOAT-EPSILON LONG-FLOAT-NEGATIVE-EPSILON
    MOST-NEGATIVE-DOUBLE-FLOAT MOST-NEGATIVE-FIXNUM MOST-NEGATIVE-LONG-FLOAT
    MOST-NEGATIVE-SHORT-FLOAT MOST-NEGATIVE-SINGLE-FLOAT
    MOST-POSITIVE-DOUBLE-FLOAT MOST-POSITIVE-FIXNUM MOST-POSITIVE-LONG-FLOAT
    MOST-POSITIVE-SHORT-FLOAT MOST-POSITIVE-SINGLE-FLOAT MULTIPLE-VALUES-LIMIT NIL
    PI SHORT-FLOAT-EPSILON SHORT-FLOAT-NEGATIVE-EPSILON SINGLE-FLOAT-EPSILON
    SINGLE-FLOAT-NEGATIVE-EPSILON T))

(defun init-smack-interp ()
  "Initialize the smacklisp interpreter with some global variables."
  ;; Define Smacklisp procedures as CL functions:
  (mapc #'link-smack-cl-function *smack-procs*)
  ;; Define the `constants'.
  (mapc #'init-smack-constant *smack-constants*)
  ;; define macros
  (initialize-system-macros)
  (initialize-system-defsetfs)
  (initialize-global-vars)
  (initialize-internal-vars))

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


(defun link-smack-cl-function (fname &optional cl-name)
  "Define a Smacklisp function as a corresponding CL function."
  (let ((smack-name (ensure-car fname))
        (cl-name (if (listp fname)
                     (second fname)
                     (if cl-name cl-name fname))))
    (set-global-func smack-name (symbol-function cl-name))))

(defun init-smack-constant (c)
  (if (listp c)
      (smack-defconstant (first c) (symbol-value (second c)))
      (smack-defconstant c (symbol-value c))))

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
        ((arv:singlep exps) (first exps))
        (t (cons op exps))))




(defun load-stream (stream)
  (unless (smackprop-p t 'global-val)
    (init-smack-interp))
  (do ((s (read stream nil :eof nil)
          (read stream nil :eof nil)))
      ((eq s :eof))
    (print (interp s nil nil))))

(defun load-string (string)
  (with-input-from-string (stream string)
    (load-stream stream)))

(defun load-file (file)
  (with-input-from-file (stream file)
    (load-stream stream)))
       









(defun cxxr ()
  (let ((l nil))
    (do-all-symbols (x) 
      (when (and (fboundp x)
                 (eq (find-package 'common-lisp)
                     (symbol-package x))
                 ((lambda (x)
                    (and (eq (first-elt x) #\C)
                         (eq (last-elt x) #\R)
                         (every #'(lambda (c) (or (eq c #\A) (eq c #\D)))
                                (subseq x 1 (1- (length x))))))
                  (string x)))
        (push x l)))
    (stable-sort (sort (remove-duplicates l) #'string< )
                 #'< :key (compose #'length #'string))))
  
  
