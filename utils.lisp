(in-package :smacklisp)

;; copied from arvid-utils
;; alexandria has similar for sequences
;; stolen from paip
(defun list-of-length-p (l n)
  (if (= n 0) 
      (null l)
      (and (consp l)
           (list-of-length-p (rest l) (1- n)))))

(defun singlep (l) 
  (and (consp l) (null (rest l))))

(defun doublep (l)
  (and (consp l) (singlep (rest l))))

;; copied from arvid-utils
;; from my own idiot brain
;; maybe should be macro? not likely to map/funcall this.
(defun make-adjustable-string (size &key (element-type 'character)
                                         (initial-element #\Space)
                                         (initial-contents nil initial-contents-p))
  "similar to make-string "
  (if initial-contents-p
        (make-array (list size)
              :initial-contents initial-contents
              :element-type element-type
              :fill-pointer t
              :adjustable t)
        (make-array (list size)
               :initial-element initial-element
               :element-type element-type
               :fill-pointer t
               :adjustable t)))

;; wrapper macro for named-readtables
(defmacro with-named-readtable ((name) &body body)
  `(let ((*readtable* (ensure-readtable ,name)))
     ,@body))

