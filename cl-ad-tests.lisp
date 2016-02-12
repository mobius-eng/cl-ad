(defpackage :cl-ad-tests
  (:shadowing-import-from
   #:cl-ad
   #:+ #:- #:* #:/
   #:sin #:cos #:tan #:asin #:acos #:atan
   #:exp #:expt #:log #:sqrt
   #:sinh #:cosh #:tanh
   #:= #:< #:> #:<= #:>= #:/=
   #:zerop #:plusp #:minusp #:numberp
   #:incf #:decf #:1+ #:1-)
  (:use #:cl #:fiveam #:cl-ad))


(in-package cl-ad-tests)

(def-suite cl-ad-tests
    :description "Test suite for automatic differentiation")

(in-suite cl-ad-tests)

(defvar *precision* (sqrt double-float-epsilon))

(defun num= (x y &optional (precision *precision*))
  (< (abs (- x y)) (* precision (max (abs x) (abs y)))))

(defun random-vector (length &optional (boundary 1.0d0))
  (let ((arr (make-array length :initial-element 0)))
    (dotimes (i length arr)
      (setf (svref arr i) (- (random boundary) (/ boundary 2d0))))))



(test scalar-function-derivative
  "Derivative of the scalar function
                    2
    f(x) = x * sin x  + 3 exp x * tan x

"
  (flet ((scalar-f (x)
           (+ (* x (sin (expt x 2))) (* 3 (exp x)) (tan x)))
         (d-scalar-f (x)
           (+ (sin (expt x 2))
              (* 2 (expt x 2) (cos (expt x 2)))
              (* 3 (exp x))
              1
              (expt (tan x) 2)))
         (d^2-scalar-f (x)
           (+ (* 2 x (cos (expt x 2)))
              (* 4 x (cos (expt x 2)))
              (* -4 (expt x 3) (sin (expt x 2)))
              (* 3 (exp x))
              (* 2 (tan x) (+ 1 (expt (tan x) 2))))))
    (let* ((df (D #'scalar-f))
           (d2f (D df))
           (x-vector (random-vector 100))
           (df-vector (map 'list df x-vector))
           (df-exact (map 'list #'d-scalar-f x-vector))
           (d2f-vector (map 'list d2f x-vector))
           (d2f-exact (map 'list #'d^2-scalar-f x-vector)))
      (is (every #'num= df-vector df-exact))
      (is (every #'num= d2f-vector d2f-exact)))))

;; (run! 'scalar-function-derivative)

(defmacro with-vector-items (items vector &body body)
  "Another approach to destructure vectors"
  (let ((gv (gensym "VECTOR")))
    (let ((bindings (mapcar (lambda (var-index)
                              (destructuring-bind (var index) var-index
                                `(,var (aref ,gv ,index))))
                            items)))
      `(let ((,gv ,vector))
         (symbol-macrolet (,@bindings)
           ,@body)))))

(test gradient-forward-of-multi-f
  "Gradient of multivariant function

                            2
  f(x, y, z) = x * sin y + y  * cos z
"
  (flet ((f (v)
           (with-vector-items ((x 0) (y 1) (z 2)) v
               (+ (* x (sin y)) (* (expt y 2) (cos z)))))
         (grad-f (v)
           (with-vector-items ((x 0) (y 1) (z 2)) v
             (vector (sin y)
                    (+ (* x (cos y)) (* 2 y (cos z)))
                    (* (expt y 2) (- (sin z)))))))
    (let* ((input-vectors (loop :for i :from 1 :upto 10
                             :collect (random-vector 3 10.0d0)))
           (df (gradient-forward #'f))
           (results (mapcar df input-vectors))
           (exacts (mapcar #'grad-f input-vectors)))
      (is (every (lambda (u v) (every #'num= u v)) results exacts)))))

;; (run! 'gradient-forward-of-multi-f)

(defun mmul (matrix vector result)
  (let ((ncols (length vector))
        (nrows (length result)))
    (dotimes (i nrows)
      (setf (aref result i) 0)
      (dotimes (j ncols)
        (incf (aref result i) (* (aref matrix i j) (aref vector j)))))))


(test jacobian*vector-of-function
  "Jacobian of a function

              /                           \
              | sin(cos x) / (y + 5)      |
              |                           |
    F(x,y) =  | log (y + 7) * exp (0.1 x) |
              |                           |
              | tan (x * y)               |
              \                           /



               /                                                          2 \
               |   -sin(x)cos(cos(x)) / (y + 5)    - sin(cos(x)) / (y + 5)  |
               |                                                            |
   J[F](x,y) = | 0.1 exp (0.1 * x) * log (y + 7)    exp(0.1 * x) / (y+7)    |
               |                                                            |
               |                       2                              2     |
               |      y / (cos (x * y))              x / (cos (x * y))      |
               \                                                            /
"
  (flet ((f (v yout)
           (with-vector-items ((x 0) (y 1)) v
             (with-vector-items ((p 0) (q 1) (r 2)) yout
               (setf p (/ (sin (cos x)) (+ y 5)))
               (setf q (* (log (+ y 7)) (exp (* 0.1 x))))
               (setf r (tan (* x y))))))
         (jac (v m)
           (with-vector-items ((x 0) (y 1)) v
             (setf (aref m 0 0) (- (/ (* (sin x) (cos (cos x))) (+ y 5)))
                   (aref m 0 1) (- (/ (sin (cos x)) (expt (+ y 5) 2)))
                   (aref m 1 0) (* 0.1 (exp (* 0.1 x)) (log (+ y 7)))
                   (aref m 1 1) (/ (exp (* 0.1 x)) (+ y 7))
                   (aref m 2 0) (/ y (expt (cos (* x y)) 2))
                   (aref m 2 1) (/ x (expt (cos (* x y)) 2))))))
    (let* ((point (random-vector 2 3.0d0))
           (direction (random-vector 2 7.0d0))
           (j*v (jacobian*vector #'f (vector 0 0) (vector 0 0 0)))
           (result (vector 0 0 0))
           (jac-matrix (make-array '(3 2)))
           (exact (vector 0 0 0)))
      (jac point jac-matrix)
      (mmul jac-matrix direction exact)
      (funcall j*v point direction result)
      (is (every #'num= result exact)))))

;; (run! 'jacobian*vector-of-function)

(run! 'cl-ad-tests)
