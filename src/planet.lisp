;;; planet.lisp

(defpackage :planet
  (:use :cl :iter)
  (:export :make-atom-feed
           :make-rss-2.0-feed
           :planet
           :defplanet
           :planet-load-all
           :planet-syndicate-feed
           :planet-clear))

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

;;  (local-time:timestamp-to-universal (local-time:parse-timestring (slot-value entry 'published))))
   
(defclass feed ()
  ((author :initarg :author :initform nil :reader feed-author)
   (url :initarg :url :reader feed-url)
   (find-entry-xpath-query :initarg :find-entry-xpath-query :reader feed-find-entry-xpath-query)
   (parse-entry :initarg :parse-entry :reader feed-parse-entry)))
  
(defclass planet ()
  ((name :initarg :name :initform "PLANET" :accessor planet-name)
   (alternate-href :initarg :alternate-href :initform nil :accessor planet-alternate-href)
   (self-href :initarg :self-href :initform nil :accessor planet-self-href)
   (id :initarg :id :initform nil :accessor planet-id)
   (feeds :initarg :feeds :initform nil :accessor planet-feeds)
   (syndicate-feed :initform nil :reader planet-syndicate-feed)
   (scheduler :initform nil)))

(defun planet-load-all (planet)
  "Load all feeds"
  (with-slots (syndicate-feed) planet
    (when syndicate-feed
      (xtree:release syndicate-feed))
    (let ((entries nil))
      (iter (for feed in (planet-feeds planet))
            (xtree:with-parse-document (rawfeed (puri:parse-uri (feed-url feed)))
              (iter (for rawentry in-xpath-result (feed-find-entry-xpath-query feed) on rawfeed with-ns-map *feeds-ns-map*)
                    (push (funcall (feed-parse-entry feed)
                                   rawentry
                                   (feed-author feed))
                          entries))))
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

                    (iter (for entry in (sort entries #'> :key #'entry-published-universal))
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
            (clon:schedule-function '(lambda () (planet-load-all planet))
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
  (when schedule
    (funcall #'planet-reset-scheduler
             (list* planet
                    schedule)))
  (tg:finalize planet #'planet-clear))

(defmacro defplanet (planet-name &key name alternate-href self-href (schedule '(:hour *)) feeds)
  `(progn
     (when (and (boundp ',planet-name)
                (typep ,planet-name 'planet))
       (planet-clear ,planet-name))
     (defparameter ,planet-name
       (make-instance 'planet
                      :name ,name
                      :alternate-href ,alternate-href
                      :self-href ,self-href
                      :schedule ,schedule
                      :feeds ,feeds))))
     

;;; make-atom-feed

(defun parse-atom-entry (node author)
  (flet ((find-string (expr)
           (xpath:find-string node expr :ns-map *feeds-ns-map*)))
    (make-instance 'entry
                   :title (find-string "atom:title")
                   :link (find-string "atom:link[@rel = 'alternate' or not(@rel)]/@href")
                   :id (find-string "atom:id")
                   :published (local-time:timestamp-to-universal (local-time:parse-timestring (find-string "atom:published")))
                   :updated (find-string "atom:updated")
                   :content (find-string "atom:content")
                   :author author)))

(defun make-atom-feed (author-name author-href href &key category)
  (make-instance 'feed
                 :author (make-author author-name author-href)
                 :url href
                 :find-entry-xpath-query (if category
                                             (format nil "/atom:feed/atom:entry[atom:category/@term='~A']" category)
                                             "/atom:feed/atom:entry")
                 :parse-entry #'parse-atom-entry))

;;; make-rss-2.0-feed

(defun parse-rss-item (node author)
  (flet ((find-string (expr)
           (xpath:find-string node expr :ns-map nil)))
    (make-instance 'entry
                   :title (find-string "title")
                   :link (find-string "link")
                   :id (find-string "guid")
                   :published (net.telent.date:parse-time (find-string "pubDate"))
                   :updated nil
                   :content (find-string "description")
                   :author author)))

(defun make-rss-2.0-feed (author-name author-href href &key category)
  (make-instance 'feed
                 :author (make-author author-name author-href)
                 :url href
                 :find-entry-xpath-query (if category
                                             (format nil "/rss/channel/item[category = '~A']" category)
                                             "/rss/channel/item")
                 :parse-entry #'parse-rss-item))
  