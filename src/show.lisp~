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
	(showAll (clingon:getopt cmd :show-all))
	(date (clingon:getopt cmd :show-date)))
    (cond (tomorrow
	   (format t "Showing events for ~a~%" (formatDateOnly (formatLocalTime (local-time:timestamp+ (local-time:today) 1 :day))))
	   (showEvents (formatLocalTime (local-time:timestamp+ (local-time:today) 1 :day))))
	  (showAll
	   (format t "Showing all upcoming events"))
	  (date
	   (format t "Showing events for ~a~%" date)
	   (showEvents date))
	  (t
	   (format t "Showing events for ~a~%" (formatDateOnly (formatLocalTime (local-time:now))))
	   (showEvents (formatLocalTime (local-time:now)))))))

(defun show/command ()
  (clingon:make-command
   :name "show"
   :description "Show upcoming events from the database for today"
   :options (show/options)
   :handler #'show/handler))
