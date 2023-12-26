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
    (format t "Importing file ~a.~%" file)
    (with-open-file (stream file)
      (loop for line = (read-line stream nil nil) for index from 0
	    while line
	    do (progn
		 (cond ((= 0 index)
			(checkICS line))
		       ((and (checkForBEGINEVENT line) (not inVEVENT))
			(setf inVEVENT t))
		       ((and (checkForStart line) inVEVENT)
			(setf startTimeFound t)
			(setf startTime (getStartTime line)))
		       ((and (checkForEndOrDuration line) inVEVENT)
			(setf endTimeFound t)
			(setf endTime (getEndTime startTime line)))
		       ((and (checkForSummary line) inVEVENT)
			(setf summaryFound t)
			(setf summary (getSummaryDesc line)))
		       ((and (checkForDesc line) inVEVENT)
			(setf descFound t)
			(setf desc (getSummaryDesc line)))
		       ((and (checkForENDEVENT line) inVEVENT)
			(cond ((not startTimeFound)
			       (format t "Found an event with missing start time ending at line ~a, skipping import.~%" index))
			      ((not endTimeFound)
			       (format t "Found an event with missing end time ending at line ~a, skipping import.~%" index))
			      ((not summaryFound)
			       (format t "Found an event with missing summary ending at line ~a, skipping import.~%" index))
			      (t
			       (with-standard-io-syntax
				 (if descFound
				     (setf output (format nil "~$::~$::~$::~$" startTime endTime summary desc))
				     (setf output (format nil "~$::~$::~$" startTime endTime summary)))
				 (addEvent output)
				 (format t "Imported event (start, end, summary): ~a, ~a, ~a~%" startTime endTime (displaySumOrDesc summary)))))
			(setf inVEVENT nil)
			(setf startTimeFound nil)
			(setf endTimeFound nil)
			(setf summaryFound nil)
			(setf descFound nil))))))
    (format t "Import finished.~%")))

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

(defun checkForBEGINEVENT (line)
  "Checks if the line contains a BEGIN:VEVENT property."
  (if (search "BEGIN:VEVENT" line)
      t
      nil))

(defun checkForENDEVENT (line)
  "Checks if the line contains a END:VEVENT property."
  (if (search "END:VEVENT" line)
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
  "Gets the summary or description from the line, removes LF and CR, then returns it as a string."
  (remove (code-char 13) (remove (code-char 10) (subseq (subseq line (+ (position #\: line :test #'equal) 1)) 0 (length (subseq line (+ (position #\: line :test #'equal) 1)))))))
