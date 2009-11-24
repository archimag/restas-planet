;;; planet.asd

(defpackage #:restas-planet-system
  (:use #:cl #:asdf))

(in-package #:restas-planet-system)

(defsystem restas-planet
    :depends-on (#:net-telent-date #:local-time #:clon #:restas #:closure-template #:cl-libxml2)
    :components
    ((:module :src
              :components
              ((:file "packages")
               (:file "feed-parser" :depends-on ("packages"))
               (:file "spider" :depends-on ("feed-parser"))
               (:file "planet" :depends-on ("spider"))))))
