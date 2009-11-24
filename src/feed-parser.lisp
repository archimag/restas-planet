;;;; feed-parser.lisp

(in-package :restas.planet)

(defparameter *feeds-ns-map* '(("atom" "http://www.w3.org/2005/Atom")))

(defun make-author (name uri)
  (list :name name
        :uri uri))

(defun make-entry (&key title link id published updated content author)
  (list :title title
        :link link
        :id id
        :published published
        :updated updated
        :content content
        :author author))

(defun entry-published-universal (entry)
  (local-time:timestamp-to-universal (getf entry :published)))


(defun parse-atom-feed (rawfeed &optional category)
  "Parse feed in Atom format"
  (let ((xpath-all-entities (if category
                                (format nil "/atom:feed/atom:entry[atom:category/@term='~A']" category)
                                "/atom:feed/atom:entry"))
        (author (make-author (xpath:find-string rawfeed
                                                "/atom:feed/atom:title"
                                                :ns-map *feeds-ns-map*)
                             (xpath:find-string rawfeed
                                                "/atom:feed/atom:link[@rel='alternate']/@href"
                                                :ns-map *feeds-ns-map*))))
    (cons author
          (iter (for rawentry in-xpath-result xpath-all-entities on rawfeed with-ns-map *feeds-ns-map*)
                (flet ((find-string (expr)
                         (xpath:find-string rawentry expr :ns-map *feeds-ns-map*)))
                  (collect (make-entry
                            :title (find-string "atom:title")
                            :link (find-string "atom:link[@rel = 'alternate' or not(@rel)]/@href")
                            :id (find-string "atom:id")
                            :published (local-time:parse-timestring (find-string "atom:published"))
                            :updated (find-string "atom:updated")
                            :content (find-string "atom:content")
                            :author author)))))))

(defun parse-rss-2.0-feed (rawfeed &optional category)
  "Parse feed in RSS-2.0 format"
  (let ((xpath-all-entities (if category
                                (format nil "/rss/channel/item[category = '~A']" category)
                                "/rss/channel/item"))
        (author (make-author (xpath:find-string rawfeed "/rss/channel/title")
                             (xpath:find-string rawfeed "/rss/channel/link"))))
    (cons author
          (iter (for rawentry in-xpath-result xpath-all-entities on rawfeed)
                (collect (make-entry
                          :title (xpath:find-string rawentry "title")
                          :link (xpath:find-string rawentry "link")
                          :id (xpath:find-string rawentry "guid")
                          :published (local-time:universal-to-timestamp
                                      (net.telent.date:parse-time (xpath:find-string rawentry "pubDate")))
                          :updated nil
                          :content (xpath:find-string rawentry "description")
                          :author author))))))

(defgeneric parse-feed (feed &optional category)
  (:documentation "Parse feed in any format")
  (:method (feed &optional category)
    (xtree:with-parse-document (rawfeed feed)
      (parse-feed rawfeed category))))

(defmethod parse-feed ((rawfeed xtree:document) &optional category)  
  (let* ((root-feed (xtree:root rawfeed))
         (name (xtree:local-name root-feed))
         (namespace (xtree:namespace-uri root-feed)))
    (cond 
      ((and (string= name "feed")
            (string= namespace "http://www.w3.org/2005/Atom")) (parse-atom-feed rawfeed category))
      ((and (not namespace)
            (string= name "rss")
            (string= "2.0"
                     (xtree:attribute-value root-feed
                                            "version"))) (parse-rss-2.0-feed rawfeed category))
      (t (error "not supported feed type")))))
  

(defclass feed-traits ()
  ((url :initarg :url)
   (category :initarg :category :initform nil)))

(defvar *feeds* nil)

(defun feed (href &key category)
  (push (make-instance 'feed-traits
                       :url (puri:parse-uri href)
                       :category category)
        *feeds*))

(defparameter *planet.reader.package*
  (defpackage :planet.reader
    (:use)
    (:import-from #:restas.planet #:feed)))

(defun load-feeds-traits-from-file (path)
  (let ((*feeds* nil)
        (*package* *planet.reader.package*))
      (load path)
      (nreverse *feeds*)))



