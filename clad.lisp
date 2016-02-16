(in-package clad)

(defvar *cl-no-export-symbols*
  '(cl:+ cl:- cl:* cl:/ cl:sin cl:cos cl:tan cl:asin cl:acos cl:atan
    cl:exp cl:expt cl:log cl:sqrt cl:sinh cl:cosh cl:tanh
    cl:= cl:< cl:> cl:<= cl:>= cl:/=
    cl:zerop cl:minusp cl:plusp cl:numberp
    cl:incf cl:decf cl:1+ cl:1-))

(defvar *cl-ad-symbols*
  '(+ - * / sin cos tan asin acos atan
    exp expt log sqrt sinh cosh tanh
    = < > <= >= /=
    zerop minusp plusp numberp
    incf decf 1+ 1-
    D diff gradient-forward jacobian*vector
    directional-derivative-forward
    ndirectional-derivative-forward
    partial))

(loop for symbol being each external-symbol in (find-package 'cl)
   unless (member symbol *cl-no-export-symbols*)
   do (export symbol))

(loop for symbol in *cl-ad-symbols*
   do (export symbol))
