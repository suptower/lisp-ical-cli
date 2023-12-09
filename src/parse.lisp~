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
				    (formatLocalTime (calcEndTime startTime (subseq line (+ (position #\: line :test #'equal) 1))))))
	(t (format t "FAILURE, NO DTEND OR DURATION FOUND."))))


(defun calcEndTime (startTime duration)
  ;; duration needs to be parsed into year, day, hour, minute, second
  ;; check if duration is dur-date, dur-time or dur-week
  (cond ((search "W" duration) (progn
				 ;; duration is dur-week, format is PXW, where X is a number to specify the weeks
				 ;; so we need to get the substring between P and W
				 ;; then calculate the days (weeks * 7)
				 (let ((days (* (subseq duration 0 (+ (position #\W duration :test #'equal) 1)) * 7)))
				   (progn
				     ;; timestamp can now be calculated
				     (local-time:timestamp+ (cl-date-time-parser:parse-date-time startTime))))))))
  

;; input: timestamp in iso 8601.2004 ("20130723T194223")
;; output: universal time
(defun decodeTimestamp (timestamp)
  (cl-date-time-parser:parse-date-time timestamp))
   
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

;; input: timestamp in iso 8601.2004 ("20130723T194223")
;; output: human readable time string in form dd.mm.yyyy HH:MM:SS
(defun formatISO8601Time (timestamp)
  (let ((returnVal nil))
    (format t "got timestamp: ~a~%" timestamp)
					;(setf returnVal (format NIL "~a" (local-time:format-timestring nil (local-time:universal-to-timestamp (cl-date-time-parser:parse-date-time timestamp)) :format +hrtime+ :timezone local-time:+utc-zone+)))
    (format t "parsed universal: ~a~%" (cl-date-time-parser:parse-date-time timestamp))
    (format t "formatted: ~a~%" (local-time:format-timestring nil (local-time:universal-to-timestamp (cl-date-time-parser:parse-date-time (format nil "~a" timestamp))) :format +hrtime+))
  returnVal))


;; input: standard local-time-timestamp ("2019-11-13T18:08:23.3126624+01:00")
;; output: human readable time string in form dd.mm.yyyy HH:MM:SS
(defun formatLocalTime (timestamp)
  (local-time:format-timestring nil timestamp :format +hrtime+))

(defparameter +hrtime+
  ;; 08.12.2023 15:39:41
  '((:day 2) "." (:month 2) "." (:year 4) " " (:hour 2) ":" (:min 2) ":" (:sec 2)))
