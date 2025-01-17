;;;;  START SEQUENCE FOR SBCL (default port is 4242)
;;;;  sbcl --load jucl.lisp --eval "(cl-serve :port 4243)"

(load "~/quicklisp/setup")  ;;; usually not necessary, unless ...
(ql:quickload :hunchentoot)

(in-package :cl-user)

(defun debug-ignore (c h) (declare (ignore h)) (print c) (abort))
(setf *debugger-hook* #'debug-ignore)

;;; Config
(defparameter +port+ 4242)

;;; cl-eval
;;; (ignore-errors ... ) <==>
;;; (handler-case (progn . forms)
;;; (error (condition) (values nil condition)))  
(defun cl-eval (code) (handler-case  (eval (read-from-string code))
    (error (condition) (values condition nil))))
  

 
;;; WEB server
(hunchentoot:define-easy-handler (cl-string-eval :uri "/eval") (code)
  (setf (hunchentoot:content-type*) "text/plain")
  (format nil "~A~%" (cl-eval code)))
    
(defun cl-serve (&key (port +port+))    
    (hunchentoot:start 
        (make-instance 'hunchentoot:easy-acceptor 
           :port port
           :address "localhost")))

;;; remove :address "localhost" if you wish global access.
;;; test:
;;; http://localhost:4242/eval?code=(print "Hello)
;;; http://localhost:4242/eval?code=(* 1 2 3 4 5 6)
;;; http://localhost:4242/eval?code=(list 'a 'b 'c)
;;; http://localhost:4242/eval?code=(cdr (list 'a 'b 'c))
;;; caution: e.g. (+ 1 ....) not working, + --> &
