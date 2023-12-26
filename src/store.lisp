(in-package :ical-cli)

;; https://gigamonkeys.com/book/files-and-file-io

(defun addEvent (event)
  (with-open-file (file "./event_database"
			:direction :output
			:if-exists :append
			:if-does-not-exist :create)
    (format file "~a~%" event)))

(defun deleteDatabase ()
  (if (probe-file "./event_database")
      (delete-file "./event_database")))

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
    (open "./event_database" :direction :probe :if-does-not-exist :create)
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
    (setf outputBuffer (removeDuplicateEvents (merge-sort outputBuffer)))
    (deleteDatabase)
    (open "./event_database" :direction :probe :if-does-not-exist :create)
    (loop for event in outputBuffer
	  do
	     (addEvent (encodeline event)))))

(defun isDuplicate (ev1 ev2)
  "Check if two events share the same start time, end time and summary"
  (let ((st1 (nth 0 ev1))
	(st2 (nth 0 ev2))
	(en1 (nth 1 ev1))
	(en2 (nth 1 ev2))
	(su1 (nth 2 ev1))
	(su2 (nth 2 ev2))
	(isDuplicate nil))
    (cond ((and (string= st1 st2) (string= en1 en2) (string= su1 su2))
	  (setf isDuplicate t)))
    isDuplicate))
		   
(defun removeDuplicateEvents (buffer)
  (let ((outputBuffer (list))
	(maxIndex (- (length buffer) 1))
	(dupe 0))
    (loop for event in buffer for index from 0
	  do
	     (cond ((= index 0)
		    (setf outputBuffer (append outputBuffer (list event))))
		   ((not (isDuplicate (nth (- index 1) buffer) event))
		    (setf outputBuffer (append outputBuffer (list event))))
		   (t
		    (setf dupe (+ dupe 1)))))
    (cond ((= dupe 1)
	   (format t "Removed ~a duplicate event.~%" dupe))
	  ((> dupe 1)
	   (format t "Removed ~a duplicate events.~%" dupe)))
    outputBuffer))
