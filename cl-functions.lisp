(in-package :smacklisp)

(defparameter *smack-procs*
  '(
    ;; Evaluation and Compilation
    (constantp smack-constantp)

    ;; Control and data flow    
    eq equal eql
    not
    identity
    complement
    constantly
    functionp
    values
    values-list
    (fboundp smack-fboundp)
    (fdefinition smack-fdefinition)
    (fmakunbound smack-fmakunbound)
    (apply smack-apply)
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
    (random smack-random)

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
    cons append list list* nconc
    car cdr
    caar cadr cdar cddr
    caaar caadr cadar caddr cdaar cdadr cddar cdddr
    caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
    cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
    first second third fourth fifth sixth seventh eighth ninth tenth
    rest nthcdr nth last endp tailp
    consp listp atom null
    copy-list 
    list-length 
    rplaca rplacd
    getf get-properties
    make-list
    butlast nbutlast
    revappend nreconc
    ldiff 
    (get smack-get)
    (symbol-plist smack-symbol-plist)
    (%sys-put-prop smack-put-prop)
    (%sys-putf smack-putf)
    
    ;; conses - associated lists
    acons 
    copy-alist
    pairlis

    
    ;; conses - trees
    copy-tree

    
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
    concatenate copy-seq
    elt fill 
    length make-sequence
    subseq 
    replace reverse nreverse

    
    ;; hash tables
    clrhash gethash hash-table-count hash-table-p
    hash-table-rehash-size hash-table-rehash-threshold
    hash-table-size hash-table-test
    remhash sxhash
    
    ;; reader
    ;; (read smack-read)

    ;; printer
    prin1 princ terpri print    

    ;; system construction
    (load smack-load-file)

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

(defmacro def-smack-cl-fun (names lambda-list)
  "Given a function or a list of functions with a given lambda list specification,
   creates a function (or functions) usable in the smacklisp interpreter.
   also specifies the link by pushing into the global variable *smack-procs*.
   Basically handles the translation of arguments of the type function between
   smacklisp and the underlying common lisp."
  (flet ((convert-parm (p)
           (destructuring-bind (var &optional def sup)
               (ensure-list p)
             (case var
               (key '(when key
                      (lambda (x) (funcall key denv x))))
               (predicate-1
                `(when ,var
                   (lambda (x) (apply ,var denv x))))
               (predicate-2
                `(when ,var
                   (lambda (x y) (apply ,var denv x y))))
               (predicate
                `(when ,var
                   (lambda (x &rest more) (apply ,var denv x more))))
               (function
                `(when ,var
                   (lambda (&rest args) (apply ,var denv args))))
               (test '(if test (lambda (x y) (funcall test denv x y)) #'eql))
               (t (cond (sup `(if ,sup ,var ,def))
                        (def `(if ,var ,var ,def))
                        (t var)))))))
    (multiple-value-bind (required optional rest keyword)
        (smack-parse-lambda-list lambda-list)
      (let ((req (mapcar #'convert-parm required))
            (opt (mapcar #'convert-parm optional))
            (ky (mapcan (lambda (k) (list (car k) (convert-parm (cdr k))))
                        keyword)))
        (cons 'progn
              (mapcan (lambda (name)
                        (let ((smack-name (symbolicate 'smack- name) ))
                          `((defun ,smack-name ,(cons 'denv lambda-list)
                              (,(if rest 'apply 'funcall)
                               (function ,name) ,@req ,@opt ,@ky ,@rest))
                            (push (quote (,name ,smack-name))
                                  *smack-procs*))))
                      (ensure-list names)))))))


(def-smack-cl-fun (notany every some notevery)
    (predicate first-seq &rest more-seqs))
(def-smack-cl-fun member (item list &key key test))
(def-smack-cl-fun (member-if assoc-if rassoc-if)
    (predicate-1 list &key key))
(def-smack-cl-fun (adjoin assoc rassoc)
    (item alist &key key test))
(def-smack-cl-fun (count find position)
    (item sequence &key from-end test (start 0) end key))
(def-smack-cl-fun (count-if find-if position-if)
    (predicate-1 sequence &key from-end (start 0) end key))
(def-smack-cl-fun (delete remove)
    (item sequence &key from-end test (start 0) end count key))
(def-smack-cl-fun (delete-if remove-if)
    (predicate-1 sequence &key from-end (start 0) end count key))
(def-smack-cl-fun (substitute nsubstitute)
    (newitem olditem sequence &key from-end test (start 0) end count key))
(def-smack-cl-fun (substitute-if nsubstitute-if)
    (newitem predicate-1 sequence &key from-end (start 0) end count key))
(def-smack-cl-fun (mapc maplist mapl mapcon mapcar mapcan)
    (function list &rest more-lists))
(def-smack-cl-fun map (result-type function first-seq &rest more-seqs))
(def-smack-cl-fun map-into (result-sequence function &rest more-seqs))
(def-smack-cl-fun merge (result-type sequence1 sequence2 predicate-2 &key key))
(def-smack-cl-fun mismatch
    (sequence1 sequence2 &key from-end test key (start1 0) (start2 0) end1 end2))
(def-smack-cl-fun reduce
    (function sequence &key key from-end (start 0) end initial-value))
(def-smack-cl-fun (remove-duplicates delete-duplicates)
    (sequence &key from-end test (start 0) end key))
(def-smack-cl-fun search
    (sequence1 sequence2 &key from-end test key (start1 0) (start2 0) end1 end2))
(def-smack-cl-fun (sort stable-sort) (sequence predicate-2 &key key))
(def-smack-cl-fun make-hash-table (&key test size rehash-size rehash-threshold))
(def-smack-cl-fun maphash (function hash-table))
(def-smack-cl-fun tree-equal (tree1 tree2 &key test))
(def-smack-cl-fun (subst nsubst) (new old tree &key key test))
(def-smack-cl-fun (subst-if nsubst-if) (new predicate1 tree &key key))
(def-smack-cl-fun (sublis nsublis) (alist tree &key key test))
(def-smack-cl-fun
    (subsetp intersection nintersection set-difference nset-difference
             set-exclusive-or nset-exclusive-or union nunion)
    (list1 list2 &key key test))



(defun init-smack-interp ()
  "Initialize the smacklisp interpreter with some global variables."
  ;; Define Smacklisp procedures as CL functions:
  (mapc #'link-smack-cl-function *smack-procs*)
  ;; Define the constants.
  (mapc #'init-smack-constant *smack-constants*)
  ;; define macros
  (initialize-system-macros)
  (initialize-system-defsetfs)
  (initialize-global-vars)
  (initialize-internal-vars))

;; if fname is a list then it is assumed that second element
;;    refers to a smackified function whose first parameter
;;    is the dynamic environment.
(defun link-smack-cl-function (fname &optional cl-name)
  "Define a Smacklisp function as a corresponding CL function."
  (let ((smack-name (ensure-car fname))
        (cl-name (if (listp fname)
                     (second fname)
                     (if cl-name cl-name fname))))
    (set-global-func smack-name
                     (if (listp fname)
                         (symbol-function cl-name)
                         (lambda (denv &rest args)
                           (declare (ignore denv))
                           (apply (symbol-function cl-name) args))))))
