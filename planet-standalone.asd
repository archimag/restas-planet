;;; planet-standalone.asd

(defpackage :planet-standalone-system
  (:use :cl :asdf))

(in-package :planet-standalone-system)

(defsystem :planet-standalone
  :depends-on (#:planet #:restas)
  :components ((:module :src
                       :components ((:file "standalone")))))
