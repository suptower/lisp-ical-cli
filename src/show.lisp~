(in-package :ical-cli)

(defun show/options ()
  (list
   (clingon:make-option
    :flag
    :description "Show events for today (default)"
    :short-name #\t
    :long-name "today"
    :key :show-today)
   (clingon:make-option
    :flag
    :description "Show events for tomorrow"
    :short-name #\n
    :long-name "next"
    :key :show-tomorrow)
   (clingon:make-option
    :string
    :description "Show events for given date (dd.mm.yyyy)"
    :short-name #\d
    :long-name "date"
    :key :show-date)))

(defun show/handler (cmd)
  (let ((today (clingon:getopt cmd :show-today))
	(tomorrow (clingon:getopt cmd :show-tomorrow))
	(date (clingon:getopt cmd :show-date)))
    (cond (tomorrow
	   (format t "TOMORROW~%"))
	  (date
	   (format t "DATE~%")
	   (showEvents date))
	  (t
	   (format t "TODAY~%")
	   (format t "Date is ~a~%" (formatLocalTime (local-time:now)))
	   (showEvents (formatLocalTime (local-time:now)))))))

(defun show/command ()
  (clingon:make-command
   :name "show"
   :description "Show ongoing events from the database"
   :options (show/options)
   :handler #'show/handler))
