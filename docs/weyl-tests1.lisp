(ql:quickload :weyl)

(in-package :weyl)

;;; defines one general variable
(defmacro ge-var (v) 
  `(defvar ,v (coerce ',v *general*)))

(macroexpand '(ge-var r))

(defun eval-str (s)
  (eval (read-from-string s)))

;;; defines general variables from a list 
(defun ge-vars (vl)
  (loop for i in vl
    do (eval-str (format nil "(ge-var ~a)" i))))

(defun wtype (obj) (cl::type-of obj))

(weyli::ge-variables *general*)

(ge-vars '(p q r x y z x_0 x_1 x_2 x_3))

(weyli::ge-variables *general*)

p

(wtype p)

(describe (wtype p))

(inspect (wtype (* p q)))

(defvar ge1 (expt p (* p q)))

ge1

(defvar dge1/dp (deriv ge1 p))

dge1/dp

(substitute p q dge1/dp) 

(deriv (substitute p q dge1/dp) p)

(ge-variable? p)

(ge-variable? u)

(ge-vars '(u v))

;;; make-app-function (todo: wrong in manual: make-applicable-function)
(defvar f1 (weyli::make-app-function '(u v) (+ (* 'u 'v) (* 'u 'u 'u))))

f1

(defvar df10 (deriv f1 0)) 

(defvar df11(deriv f1 1))

(wtype f1)

(wtype df10)

(apply f1 '(p q)) 

(apply (deriv f1 0) '(p q)) 

(documentation 'weyli::make-ge-variable 'function)

(documentation 'weyli::coerce 'function)

(documentation 'weyli::expand 'function)

(defun show (out)
  (sb-ext:run-program "/usr/local/bin/aamath"
                    (list  (format nil "~A" out))
                    :output *standard-output*))

(show "a/b+c^x-2")


