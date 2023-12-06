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
  (subseq line (+ (position #\: line :test #'equal) 1)))

(defun getEndTime (line)
  (cond ((search "DTEND" line) (progn
				 (subseq line (+ (position #\: line :test #'equal) 1))))
	((search "DURATION" line) (progn
				    (subseq line (+ (position #\: line :test #'equal) 1))))
	(t (format t "FAILURE, NO DTEND OR DURATION FOUND."))))

(defun calcEndTime (startTime, duration))
  

(defun decodeTimestamp (timestamp))
