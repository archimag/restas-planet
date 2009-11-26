;;;; planet.lisp
;;;;
;;;; This file is part of the restas-planet library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>


(in-package #:restas.planet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; compile view templates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (closure-template:compile-template :common-lisp-backend
                                     (merge-pathnames "src/planet.tmpl"
                                                      (asdf:component-pathname (asdf:find-system '#:restas-planet)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; preferences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *name* "PLANET")

(defvar *feeds* nil)

(defvar *spider* nil)

(defvar *suggest-mail* nil)

(defvar *schedule* '(:hour *))

(defvar *template* 'restas.planet.view:feed-html)

(restas:define-initialization (context)
  (restas:with-context context
    (when *feeds*
      (restas:context-add-variable context
                                   '*spider*
                                   (make-instance 'spider
                                                  :feeds *feeds*
                                                  :schedule *schedule*)))))

(restas:define-finalization (context)
  (let ((spider (restas:context-symbol-value context '*spider*)))
    (when spider
      (spider-stop-scheduler spider))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; implementation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *resource-dir*
  (merge-pathnames "resources/"
                   (asdf:component-pathname (asdf:find-system '#:restas-planet))))

(defun planet-path (path)
  (merge-pathnames path *resource-dir*))

(define-route planet-resources (":(file)")
  (planet-path file))

(defun prepare-planet-data ()
  (list :entry-list (spider-syndicate-feed *spider*)
        :authors (spider-feeds-authors *spider*)
        :href-atom (restas:genurl-with-host 'planet-atom)
        :href-html (restas:genurl-with-host 'planet-main)
        :name *name*
        :suggest-mail *suggest-mail*))

(define-route planet-atom ("atom.xml"
                           :content-type "application/atom+xml")
  (restas.planet.view:atom-feed (prepare-planet-data)))

(define-route planet-main ("")
  (funcall *template* (prepare-planet-data)))

