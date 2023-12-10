(in-package :ical-cli)

(defun addEvent (event)
  (with-open-file (file "./event_database.csv"
			:direction :output
			:if-exists :append
			:if-does-not-exist :create)
    (format file "~a~%" event)))
