;;; planet.lisp

(restas:define-plugin :restas-planet-plugin
  (:use :cl :iter))

(in-package :restas-planet-plugin)

(defvar *planet-resources-dir* (merge-pathnames "resources/"
                                                (asdf:component-pathname (asdf:find-system :planet))))


;; ;;; atom.xml

;;(define-simple-route

;; (defun atom.xml (route bindings)
;;   (declare (ignore route bindings))
;;   *planet-feeds*)

;; ;;; main page

;; (xslt:defxsl *planet-html-xsl*
;;     (merge-pathnames "src/planet.html.xsl" restas.planet::*planet-path*))

;; (defun main-page (route bindings)
;;   (declare (ignore route))
;;   (let ((style (make-instance 'xslt:stylesheet
;;                               :pointer (xtree::pointer *planet-html-xsl*))))
;;     (xslt:stylesheet-set-param style "baseurl" (restas.preferences:getpref :planet.baseurl))
;;     (iter (for (key . value) in bindings)
;;           (xslt:stylesheet-set-param style (string-downcase (symbol-name key)) value))
;;     (apply-overlay #u"chrome://skins/rulisp.xml"                 
;;                    (gp:object-register (xslt:transform style
;;                                                        *planet-feeds*)
;;                                        *request-pool*)
;;                    bindings)))
  

