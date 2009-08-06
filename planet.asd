;;; planet.asd

(defpackage :planet-system
  (:use :cl :asdf))

(in-package :planet-system)


(defsystem :planet
  :depends-on (#:net-telent-date #:local-time #:clon #:xfactory #:colorize)
  :components
  ((:module :src
            :components
            ((:file "planet")
             (:file "resources")))))
;;             (:file "planet-plugin" :depends-on ("planet" "resources"))))))
