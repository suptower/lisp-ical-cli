(in-package :ical-cli)

;; https://gigamonkeys.com/book/files-and-file-io

(defun addEvent (event)
  (with-open-file (file "./event_database.csv"
			:direction :output
			:if-exists :append
			:if-does-not-exist :create)
    (format file "~a~%" event)))

(defun showEvents (date)
  (if (probe-file "./event_database.csv")
      (with-open-file (file "./event_database.csv"
			    :direction :input)
	(loop for line = (read-line file nil nil) for index from 0
	      while line
	      do
		 (if (checkForDate date line)
		     (format t "Date found at index ~a.~%" index))
      (format t "The database file event_database.csv does not exist!")))


(defun checkForDate (date line)
  (if (search date line)
      t
      nil))
