(in-package :cl-user)

(defpackage matrix.util
  (:use :cl))

(in-package :matrix.util)

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar (lambda (s)
                   `(,s (gensym)))
                 syms)
     ,@body))
