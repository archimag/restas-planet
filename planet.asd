;;; planet.asd

(defpackage :planet-system
  (:use :cl :asdf))

(in-package :planet-system)


(defsystem :planet
  :depends-on (#:restas-new #:net-telent-date #:local-time #:clon)
  :components
  ((:module :src
            :components
            ((:file "planet")))))
