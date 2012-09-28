(in-package :smacklisp)


(defmacro def-smack-cl-fun (name lambda-list)
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
      `(defun ,(symbolicate 'smack- name) ,(cons 'denv lambda-list)
         (,(if rest 'apply 'funcall)
          (function ,name)
          ,@(mapcar #'convert-parm required)
          ,@(mapcar #'convert-parm optional)
          ,@(mapcan (lambda (k)
                      (list (car k) (convert-parm (cdr k))))
                    keyword)
          ,@rest)))))


(def-smack-cl-fun member (item list &key key test))
(def-smack-cl-fun member-if (predicate-1 sequence &key key))
(def-smack-cl-fun assoc (item alist &key key test))
(def-smack-cl-fun assoc-if (predicate-1 sequence &key key))
(def-smack-cl-fun rassoc (item alist &key key test))
(def-smack-cl-fun rassoc-if (predicate-1 sequence &key key))
(def-smack-cl-fun adjoin (item alist &key key test))
(def-smack-cl-fun notany (predicate first-seq &rest more-seqs))
(def-smack-cl-fun every (predicate first-seq &rest more-seqs))
(def-smack-cl-fun some (predicate first-seq &rest more-seqs))
(def-smack-cl-fun notevery (predicate first-seq &rest more-seqs))
(def-smack-cl-fun find (item sequence &key from-end test (start 0) end key))
(def-smack-cl-fun find-if (predicate-1 sequence &key from-end (start 0) end key))
(def-smack-cl-fun count (item sequence &key from-end test (start 0) end key))
(def-smack-cl-fun count-if (predicate-1 sequence &key from-end (start 0) end key))
(def-smack-cl-fun position (item sequence &key from-end test (start 0) end key))
(def-smack-cl-fun position-if (predicate-1 sequence &key from-end (start 0) end key))
(def-smack-cl-fun remove (item sequence &key from-end test (start 0) end count key))
(def-smack-cl-fun remove-if (predicate-1 sequence &key from-end (start 0) end count key))
(def-smack-cl-fun delete (item sequence &key from-end test (start 0) end count key))
(def-smack-cl-fun delete-if (predicate-1 sequence &key from-end (start 0) end count key))
(def-smack-cl-fun substitute (newitem olditem sequence &key from-end test (start 0) end count key))
(def-smack-cl-fun substitute-if (newitem predicate-1 sequence &key from-end (start 0) end count key))
(def-smack-cl-fun nsubstitute (newitem olditem sequence &key from-end test (start 0) end count key))
(def-smack-cl-fun nsubstitute-if (newitem predicate-1 sequence &key from-end (start 0) end count key))
(def-smack-cl-fun mapc (function list &rest more-lists))
(def-smack-cl-fun mapcan (function list &rest more-lists))
(def-smack-cl-fun mapcar (function list &rest more-lists))
(def-smack-cl-fun mapcon (function list &rest more-lists))
(def-smack-cl-fun mapl (function list &rest more-lists))
(def-smack-cl-fun maplist (function list &rest more-lists))
(def-smack-cl-fun map (result-type function first-seq &rest more-seqs))
(def-smack-cl-fun map-into (result-sequence function &rest more-seqs))
(def-smack-cl-fun merge (result-type sequence1 sequence2 predicate-2 &key key))
(def-smack-cl-fun mismatch (sequence1 sequence2 &key from-end test key (start1 0) (start2 0) end1 end2))
(def-smack-cl-fun reduce (function sequence &key key from-end (start 0) end initial-value))
(def-smack-cl-fun remove-duplicates (sequence &key from-end test (start 0) end key))
(def-smack-cl-fun delete-duplicates (sequence &key from-end test (start 0) end key))
(def-smack-cl-fun search (sequence1 sequence2 &key from-end test key (start1 0) (start2 0) end1 end2))
(def-smack-cl-fun sort (sequence predicate-2 &key key))
(def-smack-cl-fun stable-sort (sequence predicate-2 &key key))
(def-smack-cl-fun make-hash-table (&key test size rehash-size rehash-threshold))
(def-smack-cl-fun maphash (function hash-table))

(def-smack-cl-fun subsetp (list1 list2 &key key test))
(def-smack-cl-fun tree-equal (tree1 tree2 &key test))
(def-smack-cl-fun subst (new old tree &key key test))
(def-smack-cl-fun nsubst (new old tree &key key test))
(def-smack-cl-fun subst-if (new predicate1 tree &key test))
(def-smack-cl-fun nsubst-if (new predicate1 tree &key test))
(def-smack-cl-fun sublis (alist tree &key key test))
(def-smack-cl-fun nsublis (alist tree &key key test))

(def-smack-cl-fun intersection (list1 list2 &key key test))
(def-smack-cl-fun nintersection (list1 list2 &key key test))
(def-smack-cl-fun set-difference (list1 list2 &key key test))
(def-smack-cl-fun nset-difference (list1 list2 &key key test))
(def-smack-cl-fun set-exclusive-or (list1 list2 &key key test))
(def-smack-cl-fun nset-exclusive-or (list1 list2 &key key test))
(def-smack-cl-fun union (list1 list2 &key key test))
(def-smack-cl-fun nunion (list1 list2 &key key test))

(defparameter *smack-procs*
  '(
    ;; Evaluation and Compilation
    (constantp smack-constantp)

    ;; Control and data flow    
    eq equal eql
    not
    (notany smack-notany)
    (some smack-some)
    (every smack-every)
    (notevery smack-notevery) 
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
    (member smack-member)
    (member-if smack-member-if)
    (adjoin smack-adjoin)
    (mapc smack-mapc)
    (mapcan smack-mapcan)
    (mapcar smack-mapcar)
    (mapcon smack-mapcon)
    (mapl smack-mapl)
    (maplist smack-maplist)
    
    ;; conses - associated lists
    acons 
    copy-alist
    pairlis
    (assoc smack-assoc)
    (assoc-if smack-assoc-if)
    (rassoc smack-assoc)
    (rassoc-if smack-assoc-if)
    
    ;; conses - sets
    (subsetp smack-subsetp)
    (intersection smack-intersection)
    (nintersection smack-nintersection)
    (set-difference smack-set-difference)
    (nset-difference smack-nset-difference)
    (set-exclusive-or smack-set-exclusive-or)
    (nset-exclusive-or smack-nset-exclusive-or)
    (union smack-union)
    (nunion smack-nunion)

    ;; conses - trees
    copy-tree
    (subst smack-subst)
    (nsubst smack-nsubst)
    (subst-if smack-subst-if)
    (nsubst-if smack-nsubst-if)
    (tree-equal smack-tree-equal)
    (sublis smack-sublis)
    (nsublis smack-nsublis)
    
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
    (merge smack-merge)
    (mismatch smack-mismatch) 
    (reduce smack-reduce) 
    (remove-duplicates smack-remove-duplicates)
    (delete-duplicates smack-delete-duplicates)
    (search smack-search)
    (sort smack-sort)
    (stable-sort smack-stable-sort)
    (count smack-count)
    (count-if smack-count-if)
    (find smack-find)
    (find-if smack-find-if)
    (position smack-position)
    (position-if smack-position-if)
    (remove smack-remove)
    (remove-if smack-remove-if)
    (delete smack-delete)
    (delete-if smack-delete-if)
    (substitute smack-substitute) 
    (substitute-if smack-substitute-if)
    (nsubstitute smack-nsubstitute)
    (nsubstitute-if smack-nsubstitute-if)
    (map smack-map)
    (map-into smack-map-into)
    
    ;; hash tables
    clrhash gethash hash-table-count hash-table-p
    hash-table-rehash-size hash-table-rehash-threshold
    hash-table-size hash-table-test
    remhash sxhash
    (make-hash-table smack-make-hash-table)
    (maphash smack-maphash) 
    
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
    (quit smack-quit))

)

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
