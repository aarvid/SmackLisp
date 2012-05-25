SmackLisp Version 0.01

SmackLisp is a toy lisp-2 interpreter. Currently Smacklisp is small subset of Common Lisp. That is smacklisp code should run in any common lisp implementation but not vice versa.  Smacklisp is designed basically to be a scripting language to be used client-side on a web-site.
Long-term plans include a compiler and maybe a scheme interpreter.
Smacklisp also plans to be configurable, that is certain parts can be disabled (example: configure without looping macros to force use of recursion).

Code/ideas stolen from Norvig, PAIP and Queinnec, Lisp in Small Pieces

Currently Implemented:
  Evaluation and Compilation
    -- constantp
    -- lambda
    -- quote

  Control and data flow
    -- and 
    -- apply
    -- block/return/return-from  (not 100% compatible with CL)
    -- case
    -- complement
    -- cond
    -- constantly
    -- defconstant
    -- defparameter
    -- defun  (no &aux, no setf, declares)
    -- defvar
    -- eq, eql, equal, equalp
    -- every
    -- fboundp
    -- fdefinition (no setf)
    -- flet 
    -- fmakunbound
    -- funcall
    -- function (no setf)
    -- functionp
    -- identity
    -- if
    -- labels
    -- let, let*
    -- multiple-value-bind
    -- multiple-value-call
    -- multiple-value-list
    -- not
    -- notany
    -- notevery
    -- or
    -- prog prog* (no declares)
    -- prog1
    -- prog2
    -- progn
    -- psetf
    -- psetq
    -- setf (limited setf-able functions)
    -- setq 
    -- some
    -- tagbody/go
    -- unless
    -- when

  Structures
    -- copy-structure
    -- defstruct (no options)

  Iteration
    -- do
    -- do*
    -- dolist
    -- dotimes

  Symbols:
    -- boundp

  Numbers
    -- + - * / = < > <= >= /=
    -- decf
    -- incf
    -- abs acos acosh ash asin asinh atan atanh
    -- boole byte
    -- ceiling cis complex complexp conjugate cos cosh 
    -- decode-float denominator 
    -- evenp exp expt 
    -- fceiling ffloor float floatp float-sign floor fround ftruncate
    -- gcd 
    -- imagpart integerp isqrt 
    -- lcm log 
    -- max min minusp mod 
    -- numberp numerator 
    -- oddp
    -- parse-integer phase plusp 
    -- rational rationalize rationalp realp realpart rem round
    -- signum sin sinh sqrt 
    -- tan tanh truncate
    -- zerop
    -- 1+ 1-
  
  Conses
    -- adjoin
    -- append 
    -- atom
    -- car (setf)
    -- cdr (setf)
    -- cxxr (setf) 
    -- cons 
    -- consp 
    -- copy-alist
    -- copy-list
    -- copy-tree
    -- first, second, ... tenth (setf)
    -- last
    -- list
    -- list-length
    -- listp
    -- mapc mapcan mapcar mapcon mapl maplist
    -- member
    -- nth (setf)
    -- nthcdr
    -- null
    -- pop
    -- push
    -- pushnew
    -- rest (setf)
    -- rplaca
    -- rplacd

  Arrays
    -- adjustable-array-p
    -- adjust-array
    -- aref
    -- array-dimension
    -- array-dimensions
    -- array-displacement
    -- array-element-type
    -- array-has-fill-pointer-p
    -- array-in-bounds-p
    -- arrayp
    -- array-rank
    -- array-row-major-index
    -- array-total-size
    -- make-array
    -- row-major-aref
    -- simple-vector-p
    -- svref
    -- vector
    -- vectorp
    -- vector-pop
    -- vector-push
    -- vector-push-extend

  Strings
    -- char 
    -- make-string 
    -- schar
    -- simple-string-p
    -- string
    -- string-capitalize
    -- nstring-capitalize
    -- string-downcase
    -- nstring-downcase
    -- string-upcase
    -- nstring-upcase
    -- string-equal
    -- string-greaterp
    -- string-left-trim
    -- string-lessp
    -- string-not-equal
    -- string-not-greaterp
    -- string-not-lessp
    -- stringp
    -- string-right-trim
    -- string-trim
    -- string=
    -- string/=
    -- string<
    -- string>
    -- string<=
    -- string>=
     
  Sequences
    -- concatenate
    -- copy-seq
    -- count
    -- count-if
    -- count-if-not
    -- elt
    -- fill
    -- find
    -- find-if
    -- length
    -- make-sequence
    -- map
    -- map-into
    -- merge
    -- mismatch
    -- position
    -- position-if
    -- reduce
    -- remove
    -- delete
    -- remove-duplicates
    -- delete-duplicates
    -- remove-if
    -- delete-if
    -- replace
    -- reverse
    -- nreverse
    -- search
    -- sort
    -- stable-sort
    -- subseq
    -- substitute
    -- nsubstitute
    -- substitute-if
    -- nsubstitute-if
        
  Hash Tables
    -- clrhash
    -- gethash
    -- hash-table-count
    -- hash-table-p
    -- hash-table-rehash-size
    -- hash-table-rehash-threshold
    -- hash-table-size
    -- hash-table-test
    -- make-hash-table
    -- maphash
    -- remhash
    -- sxhash
    
  Printer
    -- prin1 
    -- princ 
    -- print
    -- terpri 

  Reader
    -- read (but not remotely)

  System Construction  
    -- load 

  Constants and Variables
    -- nil
    -- pi
    -- t
    -- * ** ***
    -- + ++ +++
    -- / // ///
    -- -
    -- all the constants in SBCL package common-lisp
       (let ((l nil))
         (do-all-symbols (x) 
           (when (and (constantp x)
                      (eq (find-package 'common-lisp)
                          (symbol-package x)))
             (push x l)))
         (sort (remove-duplicates l) 'string<))

  Non Common Lisp Function
    -- quit (exit repl loop)

Currently Lacks:
  Plan to implement:
    Types 
    Tail-recursion 
    Macros (Internal exists, user not yet) 
    Unit tests
    configurable

  Hope to implement
    Documentation strings 
    declarations (at least allow)
    CLOS 
    Streams 

  Maybe
    LOOP 
    Eval 
    Debugging 
    lambda list: &aux &allow-other-keys

  No plans to Implement
    Condition handling 
    Packages 
    Deprecated functions
    Read Macros 

