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

(defun dateInRange (start end date)
  "check if date is in range between start and end"
  (if (and (local-time:timestamp< start date) (local-time:timestamp> end date))
      t
      nil))


(defun checkForDate (date line)
  "Find out if there is an event on given date"
  (let ((startDate (createLocalFromHR (first (decodeLine line))))
	(endDate (createLocalFromHR (second (decodeLine line))))
	(timestamp (createLocalFromHR date)))
    (cond ((search date line) t)
	  ((dateInRange startDate endDate timestamp) t)
	  (t nil))))

(defun checkIfSameDay (start end)
  "Check if an the start and end time of an event are on the same day"
  (let ((startDay (subseq start 0 10))
	(endDay (subseq end 0 10)))
    (if (string= startDay endDay)
	t
	nil)))

(defun getTimes (start end)
  "Format times for display event"
  (let ((output nil)
	(timeS nil)
	(timeE nil))
    (if (checkIfSameDay start end)
	(progn
	  (setf timeS (subseq start 11 16))
	  (setf timeE (subseq end 11 16))
	  (setf output (format nil "~a - ~a" timeS timeE)))
	(progn
	  (setf timeS (subseq start 0 16))
	  (setf timeE (subseq end 0 16))
	  (setf output (format nil "~a - ~a" timeS timeE))))
    output))
    

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
	  

<<<<<<< Updated upstream
(defun displayEvents (eventList)
  (if eventList
      (progn
	(loop for event in eventList
	      do
		 (let ((details (decodeLine event)))
		   (format t "~a: ~a~%" (getTimes (first details) (second details)) (third details)))))
      (format t "No upcoming events found.~%")))
		 
=======
(defun encodeLine (list)
  "Inverse function to decodeLine"
  (let ((output nil))
    (setf output (format nil "~{~a~^::~}" list))
    output))

(defun eventIsOver (line)
  "Check if event is over"
  (let ((endDate (createLocalFromHR (second (decodeLine line)))))
    (if (local-time:timestamp< endDate (local-time:now))
	t
	nil)))

(defun cleanupDatabase ()
  "Remove all events that are over"
  (let ((outputBuffer (list)))
    (if (probe-file "./event_database")
	(with-open-file (file "./event_database"
			      :direction :input)
	  (let ((skipped 0))
	    (loop for line = (read-line file nil nil) for index from 0
		  while line
		  do
		     (if (not (eventIsOver line))
			 (setf outputBuffer (append outputBuffer (list line)))
			 (setf skipped (+ skipped 1))))
	    (if (> skipped 0)
		(format t "Cleaned up database, removed ~a old events.~%" skipped)))))
    (deleteDatabase)
    (loop for line in outputBuffer
	  do
	     (addEvent line))))

(defun sortDatabase ()
  "Sort database events by start date"
  (let ((outputBuffer (list)))
    (if (probe-file "./event_database")
	(with-open-file (file "./event_database"
			      :direction :input)
	  (loop for line = (read-line file nil nil) for index from 0
		while line
		do
		   (setf outputBuffer (append outputBuffer (list (decodeLine line)))))))
    (setf outputBuffer (merge-sort outputBuffer))
    (deleteDatabase)
    (loop for event in outputBuffer
	  do
	     (addEvent (encodeline event)))))
		   
>>>>>>> Stashed changes
