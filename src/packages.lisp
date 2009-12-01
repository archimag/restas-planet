;;;; packages.lisp
;;;;
;;;; This file is part of the restas-planet library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>


(restas:define-plugin #:restas.planet
  (:use #:cl #:iter #:restas.optional)
  (:export #:*name*
           #:*suggest-mail*
           #:*feeds*
           #:*schedule*
           #:*cache-dir*
           #:*template*))
