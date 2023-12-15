(in-package :ical-cli)

;; when importing a file, we need to check, if it is a true ICS file (has a VCALENDAR and VEVENT property)

(defun import/options ()
    (list
    (clingon:make-option
     :filepath
     :short-name #\f
     :long-name "file"
     :description "(REQUIRED) The ical file to import"
     :key :file
     :required t)))

(defun import/handler (cmd)
  (let ((file (clingon:getopt cmd :file))
	(inVEVENT nil)
	(startTime nil)
	(startTimeFound nil)
	(endTime nil)
	(endTimeFound nil)
	(summary nil)
	(summaryFound nil)
	(desc nil)
	(descFound nil)
	(output nil))
    (cleanupDatabase)
    (format t "Importing file ~a~%" file)
    (with-open-file (stream file)
      (loop for line = (read-line stream nil nil) for index from 0
	    while line
	    do (progn
		 (if (= 0 index)
		     (checkICS line))
		 (if (and (checkForVEVENT line)
			  (not inVEVENT))
		       (setf inVEVENT t))
		 (if (and (checkForStart line) inVEVENT)
		     (progn
		       (setf startTimeFound t)
		       (setf startTime (getStartTime line))))
		 (if (and (checkForEndOrDuration line) inVEVENT)
		     (progn
		       (setf endTimeFound t)
		       (setf endTime (getEndTime startTime line))))
		 (if (and (checkForSummary line) inVEVENT)
		     (progn
		       (setf summaryFound t)
		       (setf summary (getSummaryDesc line))))
		 (if (and (checkForDesc line) inVEVENT)
		     (progn
		       (setf descFound t)
		       (setf desc (getSummaryDesc line)))))))
    (if (and inVEVENT startTimeFound endTimeFound summaryFound)
	(progn
	  (with-standard-io-syntax
	    (if descFound
		(setf output (format nil "~$::~$::~$::~$" startTime endTime summary desc))
		(setf output (format nil "~$::~$::~$" startTime endTime summary)))
	    (addEvent output)
	    (format t "Imported Event (start, end, summary): ~a, ~a, ~a~%" startTime endTime summary)))
	(format t "ICS file is either missing start time, end time or summary."))))

(defun import/command ()
    (clingon:make-command
     :name "import"
     :description "Import an ical file into the database"
     :examples '(("Import Standup.ics file:" . "ical-cli import -f Standup.ics"))
     :options (import/options)
     :handler #'import/handler))

(defun checkICS (line)
  "Checks if the file is a true ICS file, if not, the program will exit."
  (if (search "BEGIN:VCALENDAR" line)
      (progn
	(format t "File passed ICS check, program will be continued.~%"))
      (progn
	(format t "The specified file to import is not a true ICS file, exiting program.~%")
	(SB-EXT:QUIT))))

(defun checkForVEVENT (line)
  "Checks if the line contains a VEVENT property."
  (if (search "BEGIN:VEVENT" line)
      t
      nil))

(defun checkForStart (line)
  "Checks if the line contains a DTSTART property."
  (if (search "DTSTART" line)
      t
      nil))

(defun checkForEndOrDuration (line)
  "Checks if the line contains a DTEND or DURATION property."
  (if (or (search "DTEND" line) (search "DURATION" line))
      t
      nil))

(defun checkForSummary (line)
  "Checks if the line contains a SUMMARY property."
  (if (search "SUMMARY" line)
      t
      nil))

(defun checkForDesc (line)
  "Checks if the line contains a DESCRIPTION property."
  (if (search "DESCRIPTION" line)
      t
      nil))

(defun getStartTime (line)
  "Gets the start time from the line and returns it as a local timestamp."
  (formatLocalTime (createLocalTimestamp (subseq line (+ (position #\: line :test #'equal) 1)))))

(defun getEndTime (startTime line)
  "Gets the end time from the line (calculates it if necessary by using duration and start time) and returns it as a local timestamp."
  (cond ((search "DTEND" line) (progn
				 (formatLocalTime (createLocalTimestamp (subseq line (+ (position #\: line :test #'equal) 1))))))
	((search "DURATION" line) (progn
				    (formatLocalTime (calcEndTime (createLocalFromHR startTime) (subseq line (+ (position #\: line :test #'equal) 1))))))
	(t (format t "FAILURE, NO DTEND OR DURATION FOUND."))))

(defun getSummaryDesc (line)
  "Gets the summary or description from the line and returns it as a string."
  (subseq (subseq line (+ (position #\: line :test #'equal) 1)) 0 (- (length (subseq line (+ (position #\: line :test #'equal) 1))) 1)))