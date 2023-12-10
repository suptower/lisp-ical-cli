(in-package :ical-cli)

(defun checkForVEVENT (line)
  (if (search "BEGIN:VEVENT" line)
      t
      nil))

(defun checkForStart (line)
  (if (search "DTSTART" line)
      t
      nil))

(defun checkForEndOrDuration (line)
  (if (or (search "DTEND" line) (search "DURATION" line))
      t
      nil))

(defun getStartTime (line)
  (formatLocalTime (createLocalTimestamp (subseq line (+ (position #\: line :test #'equal) 1)))))

(defun getEndTime (startTime line)
  (cond ((search "DTEND" line) (progn
				 (formatLocalTime (createLocalTimestamp (subseq line (+ (position #\: line :test #'equal) 1))))))
	((search "DURATION" line) (progn
				    (format t "startTime looks like this: ~a~%" (createLocalFromHR startTime))
				    (formatLocalTime (calcEndTime (createLocalFromHR startTime) (subseq line (+ (position #\: line :test #'equal) 1))))))
	(t (format t "FAILURE, NO DTEND OR DURATION FOUND."))))


(defun calcEndTime (startTime duration)
  ;; duration needs to be parsed into year, day, hour, minute, second
  ;; check if duration is dur-date, dur-time or dur-week
  (let ((endTime nil)
	(days nil)
	(hours nil)
	(mins nil)
	(secs nil))
    (cond ((search "W" duration)
	   (progn
	     ;; duration is dur-week, format is PXW, where X is a number to specify the weeks
	     ;; so we need to get the substring between P and W
	     ;; then calculate the days (weeks * 7)
	     (setf days (* (parse-integer (subseq duration 1 (position #\W duration :test #'equal))) 7))
	     (setf endTime (local-time:adjust-timestamp startTime
			     (offset :day days)))))
	  ((search "D" duration)
	   (progn
	     (setf days (parse-integer (subseq duration (+ (position #\P duration :test #'equal) 1) (position #\D duration :test #'equal))))
	     ;; duration is dur-date = dur-day [dur-time]
	     ;; dur-day = DIGIT "D"
	     ;; dur-time = "T" (dur-hour / dur-minute / dur-second)
	     ;; now find out, if dur-time is attached or if it is ONLY dur-day
	     (cond ((search "T" duration)
		    (progn
		      ;; dur-time is attached
		      (cond ((search "H" duration)
			     (progn
			       (setf hours (parse-integer (subseq duration (+ (position #\T duration :test #'equal) 1) (position #\H duration :test #'equal))))
			       (cond ((search "M" duration)
				      (progn
					(setf mins (parse-integer (subseq duration (+ (position #\H duration :test #'equal) 1) (position #\M duration :test #'equal))))
					(cond ((search "S" duration)
					       (progn
						 (setf secs (parse-integer (subseq duration (+ (position #\M duration :test #'equal) 1) (position #\S duration :test #'equal))))
						 (setf endTime (local-time:adjust-timestamp startTime
								    (offset :sec secs)
								    (offset :minute mins)
								    (offset :hour hours)
								    (offset :day days)))))
						 (t (setf endTime (local-time:adjust-timestamp startTime
								    (offset :minute mins)
								    (offset :hour hours)
								    (offset :day days)))))))
				      (t (setf endTime (local-time:adjust-timestamp startTime
							 (offset :hour hours)
							 (offset :day days)))))))
			   (t (setf endTime (local-time:adjust-timestamp startTime
					      (offset :day days)))))))
		   (t (setf endTime (local-time:adjust-timestamp startTime
				      (offset :day days)))))))
	  ((search "T" duration)
	   (progn
	     (cond ((search "H" duration)
		    (progn
		      (setf hours (parse-integer (subseq duration (+ (position #\T duration :test #'equal) 1) (position #\H duration :test #'equal))))
		      (cond ((search "M" duration)
			     (progn
			       (setf mins (parse-integer (subseq duration (+ (position #\H duration :test #'equal) 1) (position #\M duration :test #'equal))))
			       (cond ((search "S" duration)
				      (progn
					(setf secs (parse-integer (subseq duration (+ (position #\M duration :test #'equal) 1) (position #\S duration :test #'equal))))
					(setf endTime (local-time:adjust-timestamp startTime
							(offset :sec secs)
							(offset :minute mins)
							(offset :hour hours)))))
				     (t (setf endTime (local-time:adjust-timestamp startTime
							(offset :minute mins)
							(offset :hour hours)))))))
			    (t (setf endTime (local-time:adjust-timestamp startTime
					       (offset :hour hours)))))))
		   ((search "M" duration)
		    (progn
		      (setf mins (parse-integer (subseq duration (+ (position #\T duration :test #'equal) 1) (position #\M duration :test #'equal))))
		      (cond ((search "S" duration)
			     (progn
			       (setf secs (parse-integer (subseq duration (+ (position #\M duration :test #'equal) 1) (position #\S duration :test #'equal))))
			       (setf endTime (local-time:adjust-timestamp startTime
					       (offset :sec secs)
					       (offset :minute mins)))))
			    (t (setf endTime (local-time:adjust-timestamp startTime
					       (offset :minute mins)))))))
		   ((search "S" duration)
		    (progn
		      (setf secs (parse-integer (subseq duration (+ (position #\T duration :test #'equal) 1) (position #\S duration :test #'equal))))
		      (setf endTime (local-time:adjust-timestamp startTime
				      (offset :sec secs)))))
		   (t (setf endTime (format nil "FAILURE, NO CORRECT DURATION WAS GIVEN FOR CALCENDTIME. GIVEN ARG: ~a~%" duration))))))
	  (t (setf endTime (format nil "FAILURE, NO CORRECT DURATION WAS GIVEN FOR CALCENDTIME. GIVEN ARG: ~a~%" duration))))
    endTime))

(defun createLocalFromHR (timestamp)
  (let ((day (parse-integer (subseq timestamp 0 2)))
	(month (parse-integer (subseq timestamp 3 5)))
	(year (parse-integer (subseq timestamp 6 10)))
	(hour (parse-integer (subseq timestamp 11 13)))
	(minute (parse-integer (subseq timestamp 14 16)))
	(second (parse-integer (subseq timestamp 17 19))))
    (local-time:encode-timestamp 0 second minute hour day month year)))
	
   
(defun createLocalTimestamp (timestamp)
  (let ((year (parse-integer (subseq timestamp 0 4)))
	(month (parse-integer (subseq timestamp 4 6)))
	(day (parse-integer (subseq timestamp 6 8)))
	(hour nil)
	(minute nil)
	(second nil)
	(output nil))
    (progn
      ;; find out if timestamp is only date or has time attached to it
      (if (search "T" timestamp)
	  (progn
	    (setf hour (parse-integer (subseq timestamp 9 11)))
	    (setf minute (parse-integer (subseq timestamp 11 13)))
	    (setf second (parse-integer (subseq timestamp 13 15)))
	    (setf output (local-time:encode-timestamp 0 second minute hour day month year)))
	  (progn
	    (setf output (local-time:encode-timestamp 0 0 0 0 day month year)))))
    output))


;; input: standard local-time-timestamp ("2019-11-13T18:08:23.3126624+01:00")
;; output: human readable time string in form dd.mm.yyyy HH:MM:SS
(defun formatLocalTime (timestamp)
  (local-time:format-timestring nil timestamp :format +hrtime+))

;; human readable time format
(defparameter +hrtime+
  ;; 08.12.2023 15:39:41
  '((:day 2) "." (:month 2) "." (:year 4) " " (:hour 2) ":" (:min 2) ":" (:sec 2)))
