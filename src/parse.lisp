(in-package :ical-cli)

(defun calculate-end-time (start-time duration)
  "Calculates the end time of an event, given the start time and the duration.
   The duration is given in the form of a string, which is parsed into the correct format.
   The start time is given in the form of a local-time-timestamp.
   The end time is returned in the form of a local-time-timestamp."
  ;; duration needs to be parsed into year, day, hour, minute, second
  ;; check if duration is dur-date, dur-time or dur-week
  (let ((end-time nil)
	(days nil)
	(hours nil)
	(mins nil)
	(secs nil))
    (cond ((search "W" duration)
	     ;; duration is dur-week, format is PXW, where X is a number to specify the weeks
	     ;; so we need to get the substring between P and W
	     ;; then calculate the days (weeks * 7)
	     (setf days (* (parse-integer (subseq duration 1 (position #\W duration :test #'equal))) 7))
	     (setf end-time (local-time:adjust-timestamp start-time
			     (offset :day days))))
	  ((search "D" duration)
	     (setf days (parse-integer (subseq duration (+ (position #\P duration :test #'equal) 1) (position #\D duration :test #'equal))))
	     ;; duration is dur-date = dur-day [dur-time]
	     ;; dur-day = DIGIT "D"
	     ;; dur-time = "T" (dur-hour / dur-minute / dur-second)
	     ;; now find out, if dur-time is attached or if it is ONLY dur-day
	     (cond ((search "T" duration)
		      ;; dur-time is attached
		      (cond ((search "H" duration)
			       (setf hours (parse-integer (subseq duration (+ (position #\T duration :test #'equal) 1) (position #\H duration :test #'equal))))
			       (cond ((search "M" duration)
					(setf mins (parse-integer (subseq duration (+ (position #\H duration :test #'equal) 1) (position #\M duration :test #'equal))))
					(cond ((search "S" duration)
						 (setf secs (parse-integer (subseq duration (+ (position #\M duration :test #'equal) 1) (position #\S duration :test #'equal))))
						 (setf end-time (local-time:adjust-timestamp start-time
								    (offset :sec secs)
								    (offset :minute mins)
								    (offset :hour hours)
								    (offset :day days))))
						 (t (setf end-time (local-time:adjust-timestamp start-time
								    (offset :minute mins)
								    (offset :hour hours)
								    (offset :day days))))))
				      (t (setf end-time (local-time:adjust-timestamp start-time
							 (offset :hour hours)
							 (offset :day days))))))
			   (t (setf end-time (local-time:adjust-timestamp start-time
					      (offset :day days)))))))
		   (t (setf end-time (local-time:adjust-timestamp start-time
				      (offset :day days)))))
	  ((search "T" duration)
	     (cond ((search "H" duration)
		      (setf hours (parse-integer (subseq duration (+ (position #\T duration :test #'equal) 1) (position #\H duration :test #'equal))))
		      (cond ((search "M" duration)
			       (setf mins (parse-integer (subseq duration (+ (position #\H duration :test #'equal) 1) (position #\M duration :test #'equal))))
			       (cond ((search "S" duration)
					(setf secs (parse-integer (subseq duration (+ (position #\M duration :test #'equal) 1) (position #\S duration :test #'equal))))
					(setf end-time (local-time:adjust-timestamp start-time
							(offset :sec secs)
							(offset :minute mins)
							(offset :hour hours))))
				     (t (setf end-time (local-time:adjust-timestamp start-time
							(offset :minute mins)
							(offset :hour hours))))))
			    (t (setf end-time (local-time:adjust-timestamp start-time
					       (offset :hour hours))))))
		   ((search "M" duration)
		      (setf mins (parse-integer (subseq duration (+ (position #\T duration :test #'equal) 1) (position #\M duration :test #'equal))))
		      (cond ((search "S" duration)
			       (setf secs (parse-integer (subseq duration (+ (position #\M duration :test #'equal) 1) (position #\S duration :test #'equal))))
			       (setf end-time (local-time:adjust-timestamp start-time
					       (offset :sec secs)
					       (offset :minute mins))))
			    (t (setf end-time (local-time:adjust-timestamp start-time
					       (offset :minute mins))))))
		   ((search "S" duration)
		      (setf secs (parse-integer (subseq duration (+ (position #\T duration :test #'equal) 1) (position #\S duration :test #'equal))))
		      (setf end-time (local-time:adjust-timestamp start-time
				      (offset :sec secs))))
		   (t (setf end-time (format nil "FAILURE, NO CORRECT DURATION WAS GIVEN FOR calculate-end-time. GIVEN ARG: ~a~%" duration))))
	  (t (setf end-time (format nil "FAILURE, NO CORRECT DURATION WAS GIVEN FOR calculate-end-time. GIVEN ARG: ~a~%" duration)))))
    end-time))

(defun create-local-from-hr (timestamp)
  "Creates a local-time-timestamp from a human readable timestamp."
  (let ((day (parse-integer (subseq timestamp 0 2)))
	(month (parse-integer (subseq timestamp 3 5)))
	(year (parse-integer (subseq timestamp 6 10)))
	(hour nil)
	(minute nil)
	(second nil))
    (if (> (length timestamp) 11)
	(progn
	  (setf hour (parse-integer (subseq timestamp 11 13)))
	  (setf minute (parse-integer (subseq timestamp 14 16)))
	  (setf second (parse-integer (subseq timestamp 17 19)))
	  (local-time:encode-timestamp 0 second minute hour day month year))
	(local-time:encode-timestamp 0 0 0 0 day month year))))
	

;; input: ISO8601:2004 timestamp ("20130723T194223Z")   
;; output: standard local-time-timestamp ("2019-11-13T18:08:23.3126624+01:00")
(defun create-local-timestamp (timestamp)
  "Creates a local-time-timestamp from a ISO8601:2004 timestamp."
  (let ((year (parse-integer (subseq timestamp 0 4)))
	(month (parse-integer (subseq timestamp 4 6)))
	(day (parse-integer (subseq timestamp 6 8)))
	(hour nil)
	(minute nil)
	(second nil)
	(output nil))
      ;; find out if timestamp is only date or has time attached to it
      (cond ((search "T" timestamp)
	    	(setf hour (parse-integer (subseq timestamp 9 11)))
	    	(setf minute (parse-integer (subseq timestamp 11 13)))
	    	(setf second (parse-integer (subseq timestamp 13 15)))
	    	(setf output (local-time:encode-timestamp 0 second minute hour day month year)))
	  	(t
	    	(setf output (local-time:encode-timestamp 0 0 0 0 day month year))))
    output))


;; input: standard local-time-timestamp ("2019-11-13T18:08:23.3126624+01:00")
;; output: human readable time string in form dd.mm.yyyy HH:MM:SS
(defun format-local-time (timestamp)
  "Formats a local-time-timestamp into a human readable time string."
  (local-time:format-timestring nil timestamp :format +hrtime+))

;; input: human readable time string in form dd.mm.yyyy HH:MM:SS
;; output: human readable string in form dd.mm.yyyy
(defun format-date-only (time)
  "Formats a human readable time string into a human readable date string."
  (subseq time 0 10))

;; human readable time format (dd.mm.yyyy HH:MM:SS)
(defparameter +hrtime+
  ;; 08.12.2023 15:39:41
  '((:day 2) "." (:month 2) "." (:year 4) " " (:hour 2) ":" (:min 2) ":" (:sec 2)))
