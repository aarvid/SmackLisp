(in-package #:smacklisp)

(defun initialize-system-defsetfs ()
  (define-smack-setf car (x) (y) `(progn (rplaca ,x ,y) ,y))
  (define-smack-setf cdr (x) (y) `(progn (rplacd ,x ,y), y))
  (define-smack-setf caar (x) (y) `(progn (rplaca (car ,x) ,y) ,y))
  (define-smack-setf cdar (x) (y) `(progn (rplacd (car ,x) ,y) ,y))
  (define-smack-setf cadr (x) (y) `(progn (rplaca (cdr ,x) ,y) ,y))
  (define-smack-setf cddr (x) (y) `(progn (rplacd (cdr ,x) ,y) ,y))
  (define-smack-setf caaar (x) (y) `(progn (rplaca (caar ,x) ,y) ,y))
  (define-smack-setf cdaar (x) (y) `(progn (rplacd (caar ,x) ,y) ,y))
  (define-smack-setf cadar (x) (y) `(progn (rplaca (cdar ,x) ,y) ,y))
  (define-smack-setf cddar (x) (y) `(progn (rplacd (cdar ,x) ,y) ,y))
  (define-smack-setf caadr (x) (y) `(progn (rplaca (cadr ,x) ,y) ,y))
  (define-smack-setf cdadr (x) (y) `(progn (rplacd (cadr ,x) ,y) ,y))
  (define-smack-setf caddr (x) (y) `(progn (rplaca (cddr ,x) ,y) ,y))
  (define-smack-setf cdddr (x) (y) `(progn (rplacd (cddr ,x) ,y) ,y))
  (define-smack-setf caaaar (x) (y) `(progn (rplaca (caaar ,x) ,y) ,y))
  (define-smack-setf cdaaar (x) (y) `(progn (rplacd (caaar ,x) ,y) ,y))
  (define-smack-setf cadaar (x) (y) `(progn (rplaca (cdaar ,x) ,y) ,y))
  (define-smack-setf cddaar (x) (y) `(progn (rplacd (cdaar ,x) ,y) ,y))
  (define-smack-setf caadar (x) (y) `(progn (rplaca (cadar ,x) ,y) ,y))
  (define-smack-setf cdadar (x) (y) `(progn (rplacd (cadar ,x) ,y) ,y))
  (define-smack-setf caddar (x) (y) `(progn (rplaca (cddar ,x) ,y) ,y))
  (define-smack-setf cdddar (x) (y) `(progn (rplacd (cddar ,x) ,y) ,y))
  (define-smack-setf caaadr (x) (y) `(progn (rplaca (caadr ,x) ,y) ,y))
  (define-smack-setf cdaadr (x) (y) `(progn (rplacd (caadr ,x) ,y) ,y))
  (define-smack-setf cadadr (x) (y) `(progn (rplaca (cdadr ,x) ,y) ,y))
  (define-smack-setf cddadr (x) (y) `(progn (rplacd (cdadr ,x) ,y) ,y))
  (define-smack-setf caaddr (x) (y) `(progn (rplaca (caddr ,x) ,y) ,y))
  (define-smack-setf cdaddr (x) (y) `(progn (rplacd (caddr ,x) ,y) ,y))
  (define-smack-setf cadddr (x) (y) `(progn (rplaca (cdddr ,x) ,y) ,y))
  (define-smack-setf cddddr (x) (y) `(progn (rplacd (cdddr ,x) ,y) ,y))
  (define-smack-setf first (x) (y) `(progn (rplaca ,x ,y) ,y))
  (define-smack-setf second (x) (y) `(progn (rplaca (cdr ,x) ,y) ,y))
  (define-smack-setf third (x) (y) `(progn (rplaca (cddr ,x) ,y) ,y))
  (define-smack-setf fourth (x) (y) `(progn (rplaca (cdddr ,x) ,y) ,y))
  (define-smack-setf fifth (x) (y) `(progn (rplaca (cddddr ,x) ,y) ,y))
  (define-smack-setf sixth (x) (y) `(progn (rplaca (nthcdr 5 ,x) ,y) ,y))
  (define-smack-setf seventh (x) (y) `(progn (rplaca (nthcdr 6 ,x) ,y) ,y))
  (define-smack-setf eighth (x) (y) `(progn (rplaca (nthcdr 7 ,x) ,y) ,y))
  (define-smack-setf ninth (x) (y) `(progn (rplaca (nthcdr 8 ,x) ,y) ,y))
  (define-smack-setf tenth (x) (y) `(progn (rplaca (nthcdr 9 ,x) ,y) ,y))
  (define-smack-setf rest (x) (y) `(progn (rplacd ,x ,y) ,y))
  (define-smack-setf nth (n l) (v) `(progn (rplaca (nthcdr ,n ,l) ,v) ,v))
  (define-smack-setf get (s p &optional d)
    (v)
    (if d
        `(progn ,d (%sys-put-prop ,s ,p ,v))
        `(%sys-put-prop ,s ,p ,v)))
  (define-smack-setf-expander getf (place indicator
                                          &optional (default nil default-p))
    (multiple-value-bind (vars vals stores store-form access-form)
        (smack-get-setf-expansion place)
      (let* ((itemp (gensym)) (store (gensym)) (def (gensym)))
        (values `(,@vars ,itemp ,@(if default-p (list def) nil))
                `(,@vals ,indicator ,@(and default-p (list default)))
                `(,store)
                `(let ((,(car stores) (%sys-putf ,access-form ,itemp ,store)))
                   ,store-form
                   ,store)
                `(getf ,access-form ,itemp ,default)))))
  t)  

