;;;; show.lisp
;;;;
;;;; This file contains the show command and handles
;;;; showing events from the event database given a specific date or showing all upcoming events.

(in-package :ical-cli)

(defun show/options ()
  (list
   (clingon:make-option
    :flag
    :description "Show events for tomorrow"
    :short-name #\t
    :long-name "tomorrow"
    :key :show-tomorrow)
   (clingon:make-option
    :flag
    :description "Show all events"
    :short-name #\a
    :long-name "all"
    :key :show-all)
   (clingon:make-option
    :string
    :description "Show events for given date (dd.mm.yyyy)"
    :short-name #\d
    :long-name "date"
    :key :show-date)))

(defun show/handler (cmd)
  (let ((tomorrow (clingon:getopt cmd :show-tomorrow))
	(show-all (clingon:getopt cmd :show-all))
	(date (clingon:getopt cmd :show-date)))
    (cleanup-database)
    (sort-database)
    (cond (tomorrow
	   (format t "Showing events for ~a~%" (format-date-only (format-local-time (local-time:timestamp+ (local-time:today) 1 :day))))
	   (show-events (format-local-time (local-time:timestamp+ (local-time:today) 1 :day))))
	  (show-all
	   (format t "Showing all upcoming events.~%")
	   (show-all-events))
	  (date
	   (format t "Showing events for ~a~%" date)
	   (show-events date))
	  (t
	   (format t "Showing events for ~a~%" (format-date-only (format-local-time (local-time:now))))
	   (show-events (format-local-time (local-time:now)))))))

(defun show/command ()
  (clingon:make-command
   :name "show"
   :description "Show upcoming events from the database for today"
   :examples '(("Show events for 2nd of March 2024" . "ical-cli show -d 02.03.2024"))
   :options (show/options)
   :handler #'show/handler))

(defun show-events (date)
  (let ((event-list (list)))
    (if (probe-file "~/.event_database")
	(progn
	  (with-open-file (file "~/.event_database"
				:direction :input)
	    (loop for line = (read-line file nil nil) for index from 0
		  while line
		  do
		     (if (check-for-date-p date line)
			 (push line event-list))))
	  (display-events (reverse event-list) nil))
	(format t "The database file event_database does not exist!~%"))))

(defun show-all-events ()
  (let ((event-list (list)))
    (if (probe-file "~/.event_database")
	(progn
	  (with-open-file (file "~/.event_database"
				:direction :input
				:if-does-not-exist nil)
	    (if file
		(loop for line = (read-line file nil nil) for index from 0
		      while line
		      do
			 (push line event-list)))
	  (display-events (reverse event-list) t)))
	(format t "The database file event_database does not exist!~%"))))

(defun display-events (event-list all)
  (if event-list
      (progn
	(loop for event in event-list
	      do
		 (let ((details (decode-line event)))
		   (if (not all)
		       (format t "~a: ~a~%" (make-times (first details) (second details)) (display-sum-or-desc (third details)))
		       (format t "~a - ~a: ~a~%" (first details) (second details) (display-sum-or-desc (third details)))))))
      (format t "No upcoming events found.~%")))

(defun display-sum-or-desc (line)
  "Removes escaped chars for display and converts \n into actual linefeeds"
  (let ((output ""))
    (loop for i from 0 to (- (length line) 1)
	  do
	     (cond ((string= #\\ (char line i))
		    (cond ((string= #\n (char line (+ i 1)))
			   (setf output (concatenate 'string output (string #\linefeed))))))
		   ((string= #\n (char line i))
		    (if (not (string= #\\ (char line (- i 1))))
			(setf output (concatenate 'string output (string (char line i))))))
		   (t
		    (setf output (concatenate 'string output (string (char line i)))))))
    output))
