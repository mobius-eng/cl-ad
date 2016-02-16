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

(defpackage #:clad
  (:shadowing-import-from
   #:cl-ad
   #:+ #:- #:* #:/
   #:sin #:cos #:tan #:asin #:acos #:atan
   #:exp #:expt #:log #:sqrt
   #:sinh #:cosh #:tanh
   #:= #:< #:> #:<= #:>= #:/=
   #:zerop #:plusp #:minusp #:numberp
   #:incf #:decf #:1+ #:1-)
  (:use #:cl #:cl-ad)
  (:documentation "Replacement for standard CL package to simplify use of AD"))


(defpackage #:clad-user
  (:use #:clad)
  (:documentation "Replacement for CL-USER package to simplify use of AD"))
