;;; planet.lisp

(restas:define-plugin :restas-planet
  (:use :cl :iter))

(in-package :restas-planet)

(defvar *planet-feeds* (xtree:parse "<feed xmlns='http://www.w3.org/2005/Atom' />"))

;; register module xpath functions

(xpath:define-xpath-function universal-time (input)
  (let ((str (typecase  input
               (string input)
               (xpath:node-set (if (> (xpath:node-set-length input) 0)
                                   (xtree:text-content (xpath:node-set-at input 0)))))))
    (let ((res (or (handler-case
                       (net.telent.date:parse-time str)
                     (error nil))
                   (handler-case
                       (local-time:universal-time (local-time:parse-timestring str))
                     (error nil))
                   0)))
      (format nil "~A" res))))

(xpath:define-xpath-function rfc2822-to-rfc3339 (input)
  (let ((str (typecase  input
               (string input)
               (xpath:node-set (if (> (xpath:node-set-length input) 0)
                                   (xtree:text-content (xpath:node-set-at input 0)))))))
    (handler-case
        (local-time:format-timestring (local-time:local-time :universal (net.telent.date:parse-time str)))
      (error nil))))

(push '(universal-time "universal-time" "chrome://restas/planet/") *xpath-functions*)
(push '(rfc2822-to-rfc3339 "rfc2822-to-rfc3339" "chrome://restas/planet/") *xslt-elements*)

;; ;;; load-all-feeds

(xslt:defxsl *planet-atom-xsl*
    (merge-pathnames "src/planet.atom.xsl" (asdf:component-pathname (asdf:find-system :planet))))

;; (defun load-all-feeds ()
;;   (gp:with-garbage-pool ()
;;     (let ((raw-feeds (gp:object-register (xtree:parse (restas.preferences:getpref :planet.feeds))))
;;           (feeds nil))
;;       (xtree:process-xinclude raw-feeds)
;;       (setf feeds
;;             (xpath:with-xpath-functions ((universal-time "universal-time" "chrome://restas/planet/")
;;                                          (rfc2822-to-rfc3339 "rfc2822-to-rfc3339" "chrome://restas/planet/"))
;;               (xslt:transform *planet-atom-xsl* raw-feeds)))
;;       (if feeds
;;           (let ((old *planet-feeds*))
;;             (setf *planet-feeds* feeds)
;;             (xtree:release old))))))

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
  

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; shedule
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defvar *planet-schedule* nil)

;; (defun stop-planet-schedule ()
;;   (if *planet-schedule*
;;       (progn (sb-ext:unschedule-timer *planet-schedule*)
;;              (setf *planet-schedule* nil))
;;       (warn "*planet-schedule* has not yet been started")))
  

;; (defun start-planet-schedule  ()
;;   (if *planet-schedule*
;;       (error "*planet-schedule* has already been started")
;;       (setf *planet-schedule*
;;             (clon:schedule-function 'load-all-feeds
;;                                     (clon:make-scheduler (clon:make-typed-cron-schedule :hour '*) :allow-now-p t)
;;                                     :name "*planet-schedule*"
;;                                     :thread t))))

;; (defun restart-planet-schedule ()
;;   (stop-planet-schedule)
;;   (start-planet-schedule))




