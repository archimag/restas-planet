;;;; demo.lisp

(asdf:operate 'asdf:load-op '#:restas-planet)

(restas:defsite #:myplanet
  (:use #:cl))

(in-package #:myplanet)

(restas:define-site-plugin planet (#:restas.planet)
  (restas.planet:*feeds* #P"/home/archimag/development/common-lisp/rulisp/planet-feeds.lisp")
  (restas.planet:*name* "My Planet"))


(restas:start-site '#:myplanet :port 8080)
