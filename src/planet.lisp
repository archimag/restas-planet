;;; planet.lisp

(defpackage :planet
  (:use :cl :iter)
  (:export :make-atom-feed
           :make-rss-2.0-feed
           :load-feeds-from-file
           :load-planet-traits
           :planet
           :defplanet
           :planet-feeds
           :planet-name
           :planet-load-all
           :planet-syndicate-feed
           :planet-clear
           :author
           :author-name
           :author-uri
           :feed
           :feed-author
           :feed-url
           :*feeds-ns-map*
           ))

(in-package :planet)

(defparameter *feeds-ns-map* '(("atom" "http://www.w3.org/2005/Atom")))

(defclass author ()
  ((name :initarg :name :initform "" :reader author-name)
   (uri :initarg :uri :initform "" :reader author-uri)))

(defun make-author (name uri)
  (make-instance 'author
                 :name name
                 :uri uri))
  
(defclass entry ()
  ((title :initarg :title :initform "")
   (link :initarg :link :initform nil )
   (id :initarg :id :initform nil)
   (published :initarg :published :initform nil)
   (updated :initarg :updated :initform nil)
   (content :initarg :content :initform "")
   (author :initarg :author :initform nil)))

(defun entry-published-universal (entry)
  (slot-value entry 'published))

(defclass feed ()
  ((author :initarg :author :initform nil :reader feed-author)
   (url :initarg :url :reader feed-url)
   (category :initarg :category :initform nil :reader feed-category)))

(defgeneric find-feed-entities (feed rawfeed))
(defgeneric parse-feed-entry (feed rawentry))

(defclass atom-feed (feed) ())

(defmethod find-feed-entities ((feed atom-feed) rawfeed)
  (xpath:eval-expression rawfeed
                         (with-slots (category) feed
                           (if category
                               (format nil "/atom:feed/atom:entry[atom:category/@term='~A']" category)
                               "/atom:feed/atom:entry"))
                         :ns-map *feeds-ns-map*))

(defmethod parse-feed-entry ((feed atom-feed) rawentry)
  (flet ((find-string (expr)
           (xpath:find-string rawentry expr :ns-map *feeds-ns-map*)))
    (let ((author (feed-author feed)))
      (make-instance 'entry
                     :title (find-string "atom:title")
                     :link (find-string "atom:link[@rel = 'alternate' or not(@rel)]/@href")
                     :id (find-string "atom:id")
                     :published (local-time:timestamp-to-universal (local-time:parse-timestring (find-string "atom:published")))
                     :updated (find-string "atom:updated")
                     :content (find-string "atom:content")
                     :author author))))

(defclass rss-2.0-feed (feed) ())

(defmethod find-feed-entities ((feed rss-2.0-feed) rawfeed)
  (xpath:eval-expression rawfeed
                         (with-slots (category) feed
                           (if category
                               (format nil "/rss/channel/item[category = '~A']" category)
                               "/rss/channel/item"))))

(defmethod parse-feed-entry ((feed rss-2.0-feed) rawentry)
  (flet ((find-string (expr)
           (xpath:find-string rawentry expr :ns-map nil)))
    (let ((author (feed-author feed)))
      (make-instance 'entry
                     :title (find-string "title")
                     :link (find-string "link")
                     :id (find-string "guid")
                     :published (net.telent.date:parse-time (find-string "pubDate"))
                     :updated nil
                     :content (find-string "description")
                     :author author))))
  
(defun update-feed-class (feed rawfeed)
  (let* ((root-feed (xtree:root rawfeed))
         (name (xtree:local-name root-feed))
         (namespace (xtree:namespace-uri root-feed)))
    (cond 
      ((and (string= namespace "http://www.w3.org/2005/Atom")
            (string= name "feed"))
       (progn
         (setf (slot-value feed 'author)
               (make-author (xpath:find-string rawfeed
                                               "/atom:feed/atom:title"
                                               :ns-map *feeds-ns-map*)
                            (xpath:find-string rawfeed
                                               "/atom:feed/atom:link[@rel='alternate']/@href"
                                               :ns-map *feeds-ns-map*)))
         (change-class feed 'atom-feed)))
      ((and (not namespace)
            (string= name "rss")
            (string= (xtree:attribute-value root-feed
                                            "version")
                     "2.0"))
       (progn
         (setf (slot-value feed 'author)
               (make-author (xpath:find-string rawfeed "/rss/channel/title")
                            (xpath:find-string rawfeed "/rss/channel/link")))
         (change-class feed 'rss-2.0-feed)))
      (t (error "not supported feed type")))))

(defvar *feeds*)
(defvar *planetname*)

(defun define-feed (href &key category author)
  (push (make-instance 'feed
                       :url href
                       :category category)
        *feeds*))

(defun define-planet (name)
  (setf *planetname* name))

(defparameter *planet.reader.package*
  (defpackage :planet.reader
    (:use)
    (:import-from :planet :define-feed :define-planet)))

(defun load-feeds-from-file (path)
  (let ((*feeds* nil)
        (*package* *planet.reader.package*))
      (load path)
      *feeds*))

(defun load-planet-traits (path)
  (let ((*feeds* nil)
        (*planetname* nil)
        (*package* *planet.reader.package*))
      (load path)
      (cons *planetname* *feeds*)))
  

(defclass planet ()
  ((name :initarg :name :initform "PLANET" :accessor planet-name)
   (alternate-href :initarg :alternate-href :initform nil :accessor planet-alternate-href)
   (self-href :initarg :self-href :initform nil :accessor planet-self-href)
   (id :initarg :id :initform nil :accessor planet-id)
   (feeds :initarg :feeds :initform nil :accessor planet-feeds)
   (feeds-path :initarg :feeds-path :initform nil :reader planet-feeds-path)
   (syndicate-feed :initform nil :reader planet-syndicate-feed)
   (scheduler :initform nil)))

(defun planet-load-all (planet)
  "Load all feeds"
  (with-slots (syndicate-feed) planet
    (when syndicate-feed
      (xtree:release syndicate-feed)
      (setf syndicate-feed nil))
    (with-slots (feeds feeds-path) planet
      (when feeds-path
        (setf feeds
              (load-feeds-from-file feeds-path))))
    (let ((entries nil)
          (feeds (planet-feeds planet)))
      (iter (for feed in feeds)
            (ignore-errors
              (xtree:with-parse-document (rawfeed (puri:parse-uri (feed-url feed)))
                (update-feed-class feed rawfeed)
                (xtree:with-object (nodeset (find-feed-entities feed rawfeed))
                  (iter (for rawentry in-nodeset (xpath:xpath-object-value nodeset))
                        (ignore-errors
                          (push (parse-feed-entry feed rawentry)
                                entries)))))))
      (setf syndicate-feed
            (xfactory:with-document-factory ((atom "http://www.w3.org/2005/Atom"))
              (atom :feed
                    (atom :title
                          (xfactory:text (planet-name planet)))
                    (atom :link
                          (xfactory:attributes :rel "self" :type "text/xml" :href (planet-self-href planet)))
                    (atom :link
                          (xfactory:attributes :rel "alternate" :type "text/html" :href (planet-alternate-href planet)))
                    (atom :id
                          (xfactory:text (or (planet-id planet)
                                             (planet-alternate-href planet))))

                    (iter (for entry in (let ((s (sort entries #'> :key #'entry-published-universal)))
                                          (if (> (length s) 50)
                                              (subseq s 0 50)
                                              s)))
                          (with-slots (title link id published updated content author) entry
                            (atom :entry
                                  (atom :title (xfactory:text title))
                                  (atom :id (xfactory:text id))
                                  (atom :link
                                        (xfactory:attributes :href link))
                                  (atom :published
                                        (xfactory:text (local-time:format-timestring nil
                                                                                     (local-time:universal-to-timestamp published))))
                                  (if updated (atom :updated (xfactory:text updated)))
                                  (atom :content (xfactory:text content))
                                  (atom :author
                                        (atom :name (xfactory:text (author-name author)))
                                        (atom :uri (xfactory:text (author-uri author)))))))))))))
                              
(defun planet-stop-scheduler (planet)
  "Stop planet scheduler"
  (with-slots (scheduler) planet
    (when scheduler
      (sb-ext:unschedule-timer scheduler)
      (setf scheduler nil))))
          
(defun planet-reset-scheduler (planet &key second minute hour day-of-month month year day-of-week)
  "Reset planet scheduler"
  (planet-stop-scheduler planet)
  (with-slots (scheduler) planet
      (setf scheduler
            (clon:schedule-function #'(lambda () (planet-load-all planet))
                                    (clon:make-scheduler (clon:make-typed-cron-schedule :second second
                                                                                        :minute minute
                                                                                        :hour hour
                                                                                        :day-of-month day-of-month
                                                                                        :month month
                                                                                        :year year
                                                                                        :day-of-week day-of-week)
                                                         :allow-now-p t)
                                    :thread t))))

(defun planet-clear (planet)
  (planet-stop-scheduler planet)
  (with-slots (syndicate-feed) planet
    (when syndicate-feed
      (xtree:release syndicate-feed)
      (setf syndicate-feed nil)))
  planet)

(defmethod initialize-instance ((planet planet) &key (schedule '(:hour *)) &allow-other-keys)
  (call-next-method)
;;   (setf (slot-value planet 'syndicate-feed)
;;         (xtree:parse "<feed xmlns=\"http://www.w3.org/2005/Atom\" />"))
  (when schedule
    (apply #'planet-reset-scheduler 
           planet
           schedule))
  (tg:finalize planet #'planet-clear))

(defmacro defplanet (planet-name &key name alternate-href self-href (schedule '(:hour *)) feeds feeds-path)
  (when (and (boundp planet-name)
             (typep (symbol-value planet-name) 'planet))
    (planet-clear (symbol-value planet-name)))
  `(progn
     (defparameter ,planet-name
       (make-instance 'planet
                      :name ,name
                      :alternate-href ,alternate-href
                      :self-href ,self-href
                      :schedule ',schedule
                      :feeds ,feeds
                      :feeds-path ,feeds-path))))