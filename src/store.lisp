(in-package :ical-cli)

(defun add-event (event)
  (with-open-file (file "~/.event_database"
			:direction :output
			:if-exists :append
			:if-does-not-exist :create)
    (format file "~a~%" event)))

(defun delete-database ()
  (if (probe-file "~/.event_database")
      (delete-file "~/.event_database")))

(defun is-date-in-range (start end date)
  "check if date is in range between start and end"
  (if (and (local-time:timestamp< start date) (local-time:timestamp> end date))
      t
      nil))

(defun check-for-date (date line)
  "Find out if there is an event on given date"
  (let ((start-date (create-local-from-hr (first (decode-line line))))
	(end-date (create-local-from-hr (second (decode-line line))))
	(timestamp (create-local-from-hr date)))
    (cond ((search date line) t)
	  ((is-date-in-range start-date end-date timestamp) t)
	  (t nil))))

(defun is-same-day (start end)
  "Check if an the start and end time of an event are on the same day"
  (let ((start-day (subseq start 0 10))
	(end-day (subseq end 0 10)))
    (if (string= start-day end-day)
	t
	nil)))

(defun make-times (start end)
  "Format times for display event"
  (let ((output nil)
	(time-start nil)
	(time-end nil))
    (cond ((is-same-day start end)
	  (setf time-start (subseq start 11 16))
	  (setf time-end (subseq end 11 16)))
	(t
	  (setf timeS (subseq start 0 16))
	  (setf timeE (subseq end 0 16))))
	(setf output (format nil "~a - ~a" time-start time-end))
    output))

(defun count-double-colons (line)
  "Find the amount of double colons in the line"
  (let ((amount 0)
	(was-double-colon nil))
    (loop for i from 0 to (- (length line) 1)
	  do
	     (if (string= #\: (char line i))
		 (cond (was-double-colon
		       (setf amount (+ amount 1))
		       (setf was-double-colon nil))
		     (t (setf was-double-colon t)))
		 (setf was-double-colon nil)))
    amount))
			       

(defun find-double-colons (line)
  "Find double colons (::) in line and return list of positions"
  (let ((positions (list 0))
	(amount (count-double-colons line)))
	(loop for i from 1 to amount
	      do
		 (setf positions (append positions (list (search "::" line :start2 (+ (car (reverse positions)) 1))))))
    positions))

(defun decode-line (line)
  "Return start, end, summary and (if available) description as list"
  (let ((positions (find-double-colons line))
	(output (list)))
    (loop for i from 0 to (- (length positions) 1)
	  do
	     (if (= i 0)
		 (setf output (append output (list (subseq line (nth i positions) (nth (+ i 1) positions)))))
		 (setf output (append output (list (subseq line (+ (nth i positions) 2) (nth (+ i 1) positions)))))))
    output))

(defun encode-line (list)
  "Inverse function to decode-line"
  (let ((output nil))
    (setf output (format nil "~{~a~^::~}" list))
    output))

(defun is-event-over (line)
  "Check if event is over"
  (let ((end-date (create-local-from-hr (second (decode-line line)))))
    (if (local-time:timestamp< end-date (local-time:now))
	t
	nil)))

(defun cleanup-database ()
  "Remove all events that are over"
  (let ((output-buffer (list)))
    (if (probe-file "~/.event_database")
	(with-open-file (file "~/.event_database"
			      :direction :input)
	  (let ((skipped 0))
	    (loop for line = (read-line file nil nil) for index from 0
		  while line
		  do
		     (if (not (is-event-over line))
			 (setf output-buffer (append output-buffer (list line)))
			 (setf skipped (+ skipped 1))))
	    (if (> skipped 0)
		(format t "Cleaned up database, removed ~a old events.~%" skipped)))))
    (delete-database)
    (open "~/.event_database" :direction :probe :if-does-not-exist :create)
    (loop for line in output-buffer
	  do
	     (add-event line))))

(defun sort-database ()
  "Sort database events by start date"
  (let ((output-buffer (list)))
    (if (probe-file "~/.event_database")
	(with-open-file (file "~/.event_database"
			      :direction :input)
	  (loop for line = (read-line file nil nil) for index from 0
		while line
		do
		   (setf output-buffer (append output-buffer (list (decode-line line)))))))
    (setf output-buffer (remove-duplicate-events (merge-sort output-buffer)))
    (delete-database)
    (open "~/.event_database" :direction :probe :if-does-not-exist :create)
    (loop for event in output-buffer
	  do
	     (add-event (encode-line event)))))

(defun is-duplicate (ev1 ev2)
  "Check if two events share the same start time, end time and summary"
  (let ((st1 (nth 0 ev1))
	(st2 (nth 0 ev2))
	(en1 (nth 1 ev1))
	(en2 (nth 1 ev2))
	(su1 (nth 2 ev1))
	(su2 (nth 2 ev2))
	(is-duplicate nil))
    (cond ((and (string= st1 st2) (string= en1 en2) (string= su1 su2))
	  (setf is-duplicate t)))
    is-duplicate))
		   
(defun remove-duplicate-events (buffer)
  (let ((output-buffer (list))
	(maxIndex (- (length buffer) 1))
	(dupe 0))
    (loop for event in buffer for index from 0
	  do
	     (cond ((= index 0)
		    (setf output-buffer (append output-buffer (list event))))
		   ((not (is-duplicate (nth (- index 1) buffer) event))
		    (setf output-buffer (append output-buffer (list event))))
		   (t
		    (setf dupe (+ dupe 1)))))
    (cond ((= dupe 1)
	   (format t "Removed ~a duplicate event.~%" dupe))
	  ((> dupe 1)
	   (format t "Removed ~a duplicate events.~%" dupe)))
    output-buffer))
