(in-package cl-user)

(defpackage :cl-ad
  (:shadow #:+ #:- #:* #:/
           #:sin #:cos #:tan #:asin #:acos #:atan
           #:exp #:expt #:log #:sqrt
           #:sinh #:cosh #:tanh
           #:= #:< #:> #:<= #:>= #:/=
           #:zerop #:plusp #:minusp #:numberp
           #:incf #:decf #:1+ #:1-)
  (:import-from :alexandria #:iota)
  (:use #:cl)
  (:export #:+ #:- #:* #:/
           #:sin #:cos #:tan #:asin #:acos #:atan
           #:exp #:log #:expt #:sqrt
           #:sinh #:cosh #:tanh
           #:= #:< #:> #:<= #:>= #:/=
           #:zerop #:plusp #:minusp #:numberp
           #:1+ #:1- #:decf #:incf
           #:D #:diff #:gradient-forward #:jacobian*vector
           #:directional-derivative-forward
           #:ndirectional-derivative-forward
           #:partial)
  (:documentation
   "Automatic differentiation package"))
