(in-package cl-ad)

;; * Automatic Differentiation

;; ** Some book-keeping
(defvar *e* 0
  "Keeps track of what was differentiated")

;; ** Dual numbers
(defclass dual-number ()
  ((epsilon
    :initarg :epsilon
    :reader epsilon
    :documentation "Book-keeping parameter")
   (primal
    :initarg :primal
    :reader primal
    :documentation "Main part of a number")
   (perturbation
    :initarg :perturbation
    :reader perturbation
    :documentation "Derivative part of a number")))

;; Can print it just as a number, but it is useful to
;; keep track of all perturbations
(defmethod print-object ((obj dual-number) out)
    (print-unreadable-object (obj out :type t)
      (with-slots (epsilon primal perturbation) obj
        (format out "~D ~A ~A" epsilon primal perturbation))))
;; ** Tape
;; Not used so far
(defclass tape ()
  ((epsilon :initarg :epsilon :reader epsilon)
   (primal :initarg :primal :reader primal)
   (factors :initarg :factors :reader factors)
   (tapes :initarg :tapes :reader tapes)
   (fanout :initarg :fanout :accessor fanout)
   (sensitivity :initarg :sensitivity :accessor sensitivity)))

(defmethod print-object ((obj tape) out)
  (print-object (primal obj) out))

;; Surprisingly normal number does not have nice print-object
;; implementation
;; (defmethod print-object ((obj number) out)
;;   (prin1 obj out))

(defun dual-number-p (obj)
  (eq (type-of obj) 'dual-number))

(defun tape-p (obj)
  (eq (type-of obj) 'tape))

(defun compare-differentiable (x1 x2)
  "Compare epsilons of two generalized numbers"
  (cond ((cl:< (epsilon x1) (epsilon x2)) :<)
        ((cl:< (epsilon x2) (epsilon x1)) :>)
        (t nil)))

(defun tape (primal &key (epsilon *e*) factors tapes)
  "Construct new tape"
  (make-instance 'tape
                 :epsilon epsilon
                 :primal primal
                 :factors factors
                 :tapes tapes
                 :fanout 0
                 :sensitivity 0))

(defun dual-number (primal &key (epsilon *e*) (perturbation 1))
  "Construct new dual number"
  (make-instance 'dual-number
                 :epsilon epsilon
                 :primal primal
                 :perturbation perturbation))

(defgeneric lifted-unary (x f df-dx)
  (:documentation "Make function of real -> real to be differentiable"))

(defmethod lifted-unary ((x t) f df-dx)
  (funcall f x))

(defmethod lifted-unary ((x dual-number) f df-dx)
  (dual-number (lifted-unary (primal x) f df-dx)
               :epsilon (epsilon x)
               :perturbation (* (funcall df-dx (primal x)) (perturbation x))))

(defmethod lifted-unary ((x tape) f df-dx)
  (tape (lifted-unary (primal x) f df-dx)
        :epsilon (epsilon x)
        :factors (list (funcall df-dx (primal x)))
        :tapes (list x)))

(defun lift-unary (f df-dx)
  "Functional interface to LIFTED-UNARY"
  (lambda (x)
    (lifted-unary x f df-dx)))

(defgeneric lifted-binary (x1 x2 f df-dx1 df-dx2)
  (:documentation "Make function real x real -> real to be differentiable on each argument"))

(defmethod lifted-binary ((x1 t) (x2 t) f df-dx1 df-dx2)
  (funcall f x1 x2))

(defmethod lifted-binary ((x1 dual-number) (x2 t) f df-dx1 df-dx2)
  (dual-number (lifted-binary (primal x1) x2 f df-dx1 df-dx2)
               :epsilon (epsilon x1)
               :perturbation (* (funcall df-dx1 (primal x1) x2) (perturbation x1))))

(defmethod lifted-binary ((x1 tape) x2 f df-dx1 df-dx2)
  (tape (lifted-binary (primal x1) x2 f df-dx1 df-dx2)
        :epsilon (epsilon x1)
        :factors (list (funcall df-dx1 (primal x1) x2))
        :tapes (list x1)))

(defmethod lifted-binary (x1 (x2 dual-number) f df-dx1 df-dx2)
  (dual-number (lifted-binary x1 (primal x2) f df-dx1 df-dx2)
               :epsilon (epsilon x2)
               :perturbation(* (funcall df-dx2 x1 (primal x2))
                               (perturbation x2))))

(defmethod lifted-binary (x1 (x2 tape) f df-dx1 df-dx2)
  (tape (lifted-binary x1 (primal x2) f df-dx1 df-dx2)
        :epsilon (epsilon x2)
        :factors (list (funcall df-dx2 x1 (primal x2)))
        :tapes (list x2)))

(defmethod lifted-binary ((x1 dual-number) (x2 dual-number) f df-dx1 df-dx2)
  (case (compare-differentiable x1 x2)
    ((:<) (dual-number (lifted-binary x1 (primal x2) f df-dx1 df-dx2)
                       :epsilon (epsilon x2)
                       :perturbation (* (funcall df-dx2 x1 (primal x2))
                                        (perturbation x2))))
    ((:>) (dual-number (lifted-binary (primal x1) x2 f df-dx1 df-dx2)
                       :epsilon (epsilon x1)
                       :perturbation (* (funcall df-dx1 (primal x1) x2)
                                        (perturbation x1))))
    (t (dual-number (lifted-binary (primal x1) (primal x2) f df-dx1 df-dx2)
                    :epsilon (epsilon x1)
                    :perturbation (+ (* (funcall df-dx1 (primal x1) (primal x2))
                                        (perturbation x1))
                                     (* (funcall df-dx2 (primal x1) (primal x2))
                                        (perturbation x2)))))))

;; TODO: is the last case correct?
(defmethod lifted-binary ((x1 dual-number) (x2 tape) f df-dx1 df-dx2)
  (case (compare-differentiable x1 x2)
    ((:<) (tape (lifted-binary x1 (primal x2) f df-dx1 df-dx2)
                :epsilon (epsilon x2)
                :factors (list (funcall df-dx2 x1 (primal x2)))
                :tapes (list x2)))
    ((:>) (dual-number (lifted-binary (primal x1) x2 f df-dx1 df-dx2)
                       :epsilon (epsilon x1)
                       :perturbation (* (funcall df-dx1 (primal x1) x2)
                                        (perturbation x1))))
    (t (dual-number (lifted-binary (primal x1) x2 f df-dx1 df-dx2)
                    :epsilon (epsilon x1)
                    :perturbation (* (funcall df-dx1 (primal x1) x2)
                                     (perturbation x1))))))

;; TODO: is the last case correct?
(defmethod lifted-binary ((x1 tape) (x2 dual-number) f df-dx1 df-dx2)
  (case (compare-differentiable x1 x2)
    ((:<) (dual-number (lifted-binary x1 (primal x2) f df-dx1 df-dx2)
                       :epsilon (epsilon x2)
                       :perturbation (* (funcall df-dx2 x1 (primal x2))
                                        (perturbation x2))))
    ((:>) (tape (lifted-binary (primal x1) x2 f df-dx1 df-dx2)
                :epsilon (epsilon x1)
                :factors (list (funcall df-dx1 (primal x1) x2))
                :tapes (list x1)))
    (t (tape (lifted-binary (primal x1) x2 f df-dx1 df-dx2)
             :epsilon (epsilon x1)
             :factors (list (funcall df-dx1 (primal x1) x2))
             :tapes (list x1)))))

(defmethod lifted-binary ((x1 tape) (x2 tape) f df-dx1 df-dx2)
  (case (compare-differentiable x1 x2)
    ((:<) (tape (lifted-binary x1 (primal x2) f df-dx1 df-dx2)
                :epsilon (epsilon x2)
                :factors (list (funcall df-dx2 x1 (primal x2)))
                :tapes (list x2)))
    ((:>) (tape (lifted-binary (primal x1) x2 f df-dx1 df-dx2)
                :epsilon (epsilon x1)
                :factors (list (funcall df-dx1 (primal x1) x2))
                :tapes (list x1)))
    (t (tape (lifted-binary (primal x1) (primal x2) f df-dx1 df-dx2)
             :epsilon (epsilon x1)
             :factors (list (funcall df-dx1 (primal x1) (primal x2))
                            (funcall df-dx2 (primal x1) (primal x2)))
             :tapes (list x1 x2)))))

(defun lift-binary (f df-dx1 df-dx2)
  "Functional interface for LIFTED-BINARY"
  (lambda (x1 x2)
    (lifted-binary x1 x2 f df-dx1 df-dx2)))


(defun lift-multi* (f df-dx1 df-dx2 xs)
  "Make function of 0 or many real arguments differentiable.
It is assumed (F Z) just leaves Z intact"
  (if (null xs)
      (funcall f)
      (reduce (lift-binary f df-dx1 df-dx2) xs)))

(defun lift-multi+ (f df-dx df-dx1 df-dx2 x xs)
  "Make function of 1 or more real arguments differentiable."
  (cond ((null xs) (lifted-unary x f df-dx))
        (t (reduce (lift-binary f df-dx1 df-dx2) xs :initial-value x))))

(defun primal* (x)
  "Find the primitive primal part of a generalized number"
  (if (cl:numberp x)
      x
      (primal* (primal x))))

(defun lift-multi-bool (f xs)
  "Apply F: number x number x ... x number -> bool to generalized numbers XS"
  (apply f (mapcar #'primal* xs)))

(defun + (&rest numbers)
  (lift-multi* #'cl:+ (constantly 1) (constantly 1) numbers))

(defun * (&rest numbers)
  (lift-multi* #'cl:*
               (lambda (x1 x2) (declare (ignore x1)) x2)
               (lambda (x1 x2) (declare (ignore x2)) x1)
               numbers))

(defun - (number &rest more-numbers)
  (lift-multi+ #'cl:-
               (constantly -1)
               (constantly 1)
               (constantly -1)
               number
               more-numbers))

(defun / (number &rest more-numbers)
  (lift-multi+ #'cl:/
               (lambda (x) (- (/ (expt x 2))))
               (lambda (x1 x2) (declare (ignore x1)) (/ x2))
               (lambda (x1 x2) (- (/ x1 (expt x2 2))))
               number
               more-numbers))

(defun sqrt (number)
  (lifted-unary number #'cl:sqrt (lambda (x) (/ 1 (* 2 (sqrt x))))))

(defun exp (number)
  (lifted-unary number #'cl:exp (lambda (x) (exp x))))

(defun log (number &optional (base nil base-p))
  (if base-p
      (lifted-unary number
                    (lambda (x) (cl:log x base))
                    (lambda (x) (/ (log (exp 1.0d0) base) x)))
      (lifted-unary number
                    (lambda (x) (cl:log x))
                    (lambda (x) (/ x)))))

(defun expt (base power)
  (lifted-binary base power
                 #'cl:expt
                 (lambda (x1 x2)
                   (let ((new-exp (- x2 1)))
                     (if (cl:= new-exp 1)
                         (* x2 x1)
                         (* x2 (expt x1 new-exp)))))
                 (lambda (x1 x2) (* (log x1) (expt x1 x2)))))

(defun sin (number)
  (lifted-unary number #'cl:sin (lambda (x) (cos x))))

(defun cos (number)
  (lifted-unary number #'cl:cos (lambda (x) (- (sin x)))))

(defun tan (number)
  (lifted-unary number #'cl:tan (lambda (x) (+ 1 (expt (tan x) 2)))))

(defun asin (number)
  (lifted-unary number #'cl:asin (lambda (x) (/ 1 (sqrt (- 1 (expt x 2)))))))

(defun acos (number)
  (lifted-unary number #'cl:acos (lambda (x) (- (/ 1 (sqrt (- 1 (expt x 2))))))))

(defun atan (number)
  (lifted-unary number #'cl:atan (lambda (x) (/ 1 (+ 1 (expt x 2))))))

(defun sinh (number)
  (lifted-unary number #'cl:sinh (lambda (x) (cosh x))))

(defun cosh (number)
  (lifted-unary number #'cl:cosh (lambda (x) (sinh x))))

(defun tanh (number)
  (lifted-unary number #'cl:tanh (lambda (x) (- 1 (expt (tanh x) 2)))))

(defun = (number &rest more-numbers)
  (lift-multi-bool #'cl:= (cons number more-numbers)))

(defun < (number &rest more-numbers)
  (lift-multi-bool #'cl:< (cons number more-numbers)))

(defun > (number &rest more-numbers)
  (lift-multi-bool #'cl:> (cons number more-numbers)))

(defun <= (number &rest more-numbers)
  (lift-multi-bool #'cl:<= (cons number more-numbers)))

(defun >= (number &rest more-numbers)
  (lift-multi-bool #'cl:>= (cons number more-numbers)))

(defun /= (number &rest more-numbers)
  (lift-multi-bool #'cl:/= (cons number more-numbers)))

(defun zerop (number)
  (lift-multi-bool #'cl:zerop (list number)))

(defun plusp (number)
  (lift-multi-bool #'cl:plusp (list number)))

(defun minusp (number)
  (lift-multi-bool #'cl:minusp (list number)))

(defun numberp (object)
  (lift-multi-bool #'cl:numberp (list object)))

(defun 1+ (number)
  (+ 1 number))

(defun 1- (number)
  (- 1 number))

(define-modify-macro decf (&optional (delta 1))
  (lambda (place &optional (delta 1))
    (- place delta)))

(define-modify-macro incf (&optional (delta 1))
  (lambda (place &optional (delta 1))
    (+ place delta)))

(defun forward-mode (map-independent map-dependent f x x-perturbation)
  "Functional way of forward mode of differentiation"
  (cl:incf *e*)
  (let ((y-forward
         (funcall f (funcall map-independent
                             (lambda (x x-perturbation)
                               (dual-number x :perturbation x-perturbation))
                             x
                             x-perturbation))))
    (cl:decf *e*)
    (funcall map-dependent
             (lambda (y-forward)
               (if (or (not (dual-number-p y-forward))
                       (cl:< (epsilon y-forward) *e*))
                   0
                   (perturbation y-forward)))
             y-forward)))

(defun forward-mode* (map-independent map-dependent f x x-perturbation dual-x y-forward y dy)
  "Imperative implementation of forward mode of differentiation

MAP-INDEPENDENT (DUAL-X OP X X-PERTURBATION): must apply OP to all X and corresponding
    X-PERTURBATION. Saves the result in DUAL-X

MAP-DEPENDENT (RESULT OP Y-FORWARD) : must apply OP to (possibly generalized numbers) in
    Y-FORWARD and put the result into RESULT

F (X YOUT): function applied to X and returns the result in YOUT.

X: point at which compute the derivative

X-PERTURBATION: perturbations for the derivative

DUAL-X: the buffer to store intermediate values of X (as dual-numbers)

Y-FORWARD: the buffer to store the result of F application (as dual-numbers)

Y: the buffer to store the primal result of F at X

DY: the buffer to store the perturbation (derivative) of F at X
"
  (flet ((make-dual-number (x x-perturbation)
           (dual-number x :perturbation x-perturbation))
         (extract-perturbation (y-forward)
           (if (or (not (dual-number-p y-forward))
                   (cl:< (epsilon y-forward) *e*))
               0
               (perturbation y-forward))))
    (cl:incf *e*)
    (funcall map-independent dual-x #'make-dual-number x x-perturbation)
    (funcall f dual-x y-forward)
    (cl:decf *e*)
    (list (lambda ()
            (funcall map-dependent y #'primal y-forward))
          (lambda ()
            (funcall map-dependent dy #'extract-perturbation y-forward)))))

(defun D (f)
  "Calculates the derivative of F as a function of a number"
  (lambda (x)
    (forward-mode (lambda (g x perturbation)
                    (funcall g x perturbation))
                  (lambda (g y-forward)
                    (funcall g y-forward))
                  f
                  x
                  1)))

(defun diff (f x)
  "Calculates the derivative of F at X"
  (forward-mode (lambda (f x perturbation) (funcall f x perturbation))
                (lambda (f y-forward) (funcall f y-forward))
                f
                x
                1))

(defun directional-derivative-forward (f)
  "Returns the function which for given point X and direction P
will return the directional derivative of F at X in direction of P:

           F(X+hP) - F(x)
  lim     ----------------
  h -> 0         h

Functional approach.
"
  (lambda (x x-perturbation)
    (forward-mode (lambda (g x x-perturbation)
                    (map 'simple-vector g x x-perturbation))
                  (lambda (g y-forward)
                    (map 'simple-vector g y-forward))
                  f
                  x
                  x-perturbation)))

(defun ndirectional-derivative-forward (f dual-x y-forward)
  "Returns the function that for given point X and direction P will
calculate the directional derivative of F at X in direction P:

           F(X+hP) - F(x)
  lim     ----------------
  h -> 0         h

Compared to DIRECTIONAL-DERIVATIVE-FORWARD, this function re-uses supplied
buffers and does not allocate extra memory.

F (X YOUT) : returns value of f(x) at X stored in YOUT.

DUAL-X : buffer used to store internal generalized arguments
Y-FORWARD : buffer used to store internal generalized results
These both are assumed to be simple-vectors

Returns a function of (X DIRECTION DYOUT):

X : the point at which to compute the derivative
DIRECTION : direction of the derivative
DYOUT : the space for the output result"
  (declare (type simple-vector dual-x y-forward))
  (let ((xsize (length dual-x))
        (ysize (length y-forward)))
    (flet ((map-independent (x-dest g x x-perturbation)
             (declare (type simple-array x x-perturbation)
                      (type simple-vector x-dest))
             (dotimes (i xsize)
               (setf (svref x-dest i) (funcall g (aref x i) (aref x-perturbation i)))))
           (map-dependent (y-dest g y-dual)
             (declare (type simple-array y-dest)
                      (type simple-vector y-dual))
             (dotimes (i ysize)
               (setf (aref y-dest i)
                     (funcall g (svref y-dual i))))))
      (lambda (x x-perturbation dy)
        (funcall
         (second (forward-mode* #'map-independent #'map-dependent
                                f
                                x x-perturbation dual-x
                                y-forward nil dy)))))))

(defun jacobian*vector (f buffer-x buffer-y)
  "Returns the function calculating the multiplication of the Jacobian of F
by a vector.

F (X YOUT) : returns the result in YOUT
BUFFER-X and BUFFER-Y : will be internal storages for independent and dependent
  variables respectively. Must be SIMPLE-VECTOR's of correct sizes

Returns the function of (X V YOUT) :

X : point at which the Jacobian is calculated (SIMPLE-ARRAY)
V : vector on which the Jacobian is multiplied (SIMPLE-ARRAY)
YOUT : the result of multiplication (SIMPLE-ARRAY)"
  (ndirectional-derivative-forward f buffer-x buffer-y))

(defun update-vector (v i x)
  "Produces a new vector based on V with Ith index replaced by X. O(n) operation"
  (let ((v1 (make-array (length v) :initial-contents v)))
    (setf (cl:svref v1 i) x)
    v1))

(defun gradient-forward (f)
  "Calculates gradient of F using forward-mode. Suboptimal for gradients!"
  (lambda (x)
    (map 'simple-vector
         (lambda (i)
           (diff (lambda (xi) (funcall f (update-vector x i xi)))
                 (svref x i)))
         (iota (length x)))))

(defun partial (n f)
  "Calculates partial derivative of F with respect to Nth variable"
  (lambda (v)
    (diff (lambda (x) (funcall f (update-vector v n x))) (svref v n))))


;; TODO: reverse derivatives!
