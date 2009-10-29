;;; resources.lisp

(defpackage :planet.resource
  (:use :cl)
  (:export #:css-file
           #:feed-icon/10x10
           #:feed-icon/14x14))

(in-package :planet.resource)

(defparameter *resource-dir* (merge-pathnames "resources/"
                                              (asdf:component-pathname (asdf:find-system :planet))))

(defun css-file ()
  (merge-pathnames "planet.css" *resource-dir*))

(defun feed-icon/10x10 ()
  (merge-pathnames "feed-icon-10x10.png" *resource-dir*))

(defun feed-icon/14x14 ()
  (merge-pathnames "feed-icon-10x10.png" *resource-dir*))
