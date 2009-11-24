;;;; packages.lisp

(restas:define-plugin #:restas.planet
  (:use #:cl #:iter)
  (:export #:*name*
           #:*feeds*
           #:*schedule*))
