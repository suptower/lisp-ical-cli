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
  (decodeTimestamp (subseq line (+ (position #\: line :test #'equal) 1))))

(defun getEndTime (line)
  (cond ((search "DTEND" line) (progn
				 (decodeTimestamp (subseq line (+ (position #\: line :test #'equal) 1)))))
	((search "DURATION" line) (progn
				    (decodeTimestamp (subseq line (+ (position #\: line :test #'equal) 1)))))
	(t (format t "FAILURE, NO DTEND OR DURATION FOUND."))))

(defun calcEndTime (startTime duration))
  

(defun decodeTimestamp (timestamp)
  (let ((year (subseq timestamp 0 4))
	(month (subseq timestamp 4 6))
	(day (subseq timestamp 6 8))
	(hour nil)
	(minute nil)
	(second nil)
	(outputString nil))
    (progn
      ;; find out if timestamp is only date or has time attached to it
      (if (search "T" timestamp)
	  (progn
	    (setf hour (subseq timestamp 9 11))
	    (setf minute (subseq timestamp 11 13))
	    (setf second (subseq timestamp 13 15))
	    (setf outputString (format NIL "~a.~a.~a ~a:~a:~a" day month year hour minute second))
	    (format t "Decoded timestamp (dd.mm.yyyy HH:MM:SS) ~a~%" outputString))
	  (progn
	    (setf outputString (format NIL "~a.~a.~a" day month year))
	    (format t "Decoded timestamp (dd.mm.yyyy) ~a~%" outputString))))
    outputString))
