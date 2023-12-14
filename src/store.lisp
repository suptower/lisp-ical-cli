(in-package :ical-cli)

;; https://gigamonkeys.com/book/files-and-file-io

(defun addEvent (event)
  (with-open-file (file "./event_database"
			:direction :output
			:if-exists :append
			:if-does-not-exist :create)
    (format file "~a~%" event)))

(defun showEvents (date)
  (let ((eventList (list)))
    (if (probe-file "./event_database")
	(progn
	  (with-open-file (file "./event_database"
				:direction :input)
	    (loop for line = (read-line file nil nil) for index from 0
		  while line
		  do
		     (if (checkForDate date line)
			 (push line eventList))))
	  (displayEvents eventList))
	(format t "The database file event_database does not exist!"))))


(defun checkForDate (date line)
  (if (search date (subseq line 0 10))
      t
      nil))

(defun checkIfSameDay (line)
  "Check if an the start and end time of an event are on the same day"
  (let ((startDay (subseq line 0 10))
	(endDay (subseq line 21 31)))
    (if (string= startDay endDay)
	t
	nil)))

(defun countDoubleColons (line)
  "Find the amount of double colons in the line"
  (let ((amount 0)
	(wasDC nil))
    (loop for i from 0 to (- (length line) 1)
	  do
	     (if (string= #\: (char line i))
		 (if wasDC
		     (progn
		       (setf amount (+ amount 1))
		       (setf wasDC nil))
		     (setf wasDC t))
		 (setf wasDC nil)))
    amount))
			       

(defun findDoubleColons (line)
  "Find double colons (::) in line and return list of positions"
  (let ((positions (list 0))
	(amount (countDoubleColons line)))
	(loop for i from 1 to amount
	      do
		 (setf positions (append positions (list (search "::" line :start2 (+ (car (reverse positions)) 1))))))
    positions))

(defun decodeLine (line)
  "Return start, end, summary and (if available) description as list"
  (let ((positions (findDoubleColons line))
	(retList (list)))
    (loop for i from 0 to (- (length positions) 1)
	  do
	     (if (= i 0)
		 (setf retList (append retList (list (subseq line (nth i positions) (nth (+ i 1) positions)))))
		 (setf retList (append retList (list (subseq line (+ (nth i positions) 2) (nth (+ i 1) positions)))))))
    retList))
	  

(defun displayEvents (eventList)
  (if eventList
      (progn
	(loop for event in eventList
	      do
		 (let ((details (decodeLine event)))
		   (format t "~a~%" details))))
      (format t "No upcoming events found.~%")))
		 
