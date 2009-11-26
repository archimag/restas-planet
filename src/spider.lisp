;;;; spider.lisp
;;;;
;;;; This file is part of the restas-planet library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>


(in-package #:restas.planet)

(defclass spider ()
  ((feeds :initarg :feeds :initform nil)
   (feeds-authors :initform nil :reader spider-feeds-authors)
   (cache-dir :initarg cache-dir :initform nil)
   (syndicate-feed :initform nil :reader spider-syndicate-feed)
   (scheduler :initform nil)))
           

(defun spider-feeds-traits (spider)
  (let ((slot-feeds (slot-value spider 'feeds)))
    (typecase slot-feeds
      (cons slot-feeds)
      (pathname (load-feeds-traits-from-file slot-feeds))
      (otherwise (error "Bad type of spider feeds: ~A" (type-of slot-feeds))))))

(defun spider-load-all-feeds (spider)
  (let ((authors nil)
        (items nil))
    (iter (for traits in (spider-feeds-traits spider))
          (let ((res (parse-feed (slot-value traits 'url)
                                 (slot-value traits 'category))))
            (push (car res)
                  authors)
            (setf items
                  (concatenate 'list
                               items
                               (cdr res)))))
    (setf (slot-value spider 'feeds-authors)
          (nreverse authors))
    (setf (slot-value spider 'syndicate-feed)
          (iter (for item in (sort items #'> :key #'entry-published-universal))
                (for i from 0 below 50)
                (collect item)))
    spider))

(defun spider-stop-scheduler (spider)
  "Stop spider scheduler"
  (with-slots (scheduler) spider
    (when scheduler
      (sb-ext:unschedule-timer scheduler)
      (setf scheduler nil))))
          
(defun spider-reset-scheduler (spider &key second minute hour day-of-month month year day-of-week)
  "Reset spider scheduler"
  (spider-stop-scheduler spider)
  (with-slots (scheduler) spider
      (setf scheduler
            (clon:schedule-function #'(lambda () (spider-load-all-feeds spider))
                                    (clon:make-scheduler (clon:make-typed-cron-schedule :second second
                                                                                        :minute minute
                                                                                        :hour hour
                                                                                        :day-of-month day-of-month
                                                                                        :month month
                                                                                        :year year
                                                                                        :day-of-week day-of-week)
                                                         :allow-now-p t)
                                    :thread t))))

(defmethod initialize-instance :after ((spider spider) &key (schedule '(:hour *)) &allow-other-keys)
  (when schedule
    (apply #'spider-reset-scheduler 
           spider
           schedule)))
