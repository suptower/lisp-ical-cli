(in-package :ical-cli)

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
  (let ((eventList (createEventList (createListFromFile (clingon:getopt cmd :file)))))
    (cleanupDatabase)
    (format t "Importing file ~a.~%" (clingon:getopt cmd :file))
	(format t "Found ~a events in file.~%" (length eventList))
	(loop for event in eventList do
	  (let ((startTime (nth 0 event))
		(endTime (nth 1 event))
		(summary (nth 2 event))
		(desc (nth 3 event)))
	    (format t "Imported event (start, end, summary): ~a, ~a, ~a~&" startTime endTime (displaySumOrDesc summary))
	    (cond ((not desc)
		   (addEvent (format nil "~$::~$::~$" startTime endTime summary)))
		  (t
		   (addEvent (format nil "~$::~$::~$::~$" startTime endTime summary desc))))))
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

(defun getSummaryDesc (file index)
  "Gets the summary or description from the line, checks for continous lines, removes LF and CR, then returns it as a string."
  (let ((line (unfoldLines file index)))
    (remove (code-char 13) (remove (code-char 10) (subseq (subseq line (+ (position #\: line :test #'equal) 1)) 0 (length (subseq line (+ (position #\: line :test #'equal) 1))))))))

(defun unfoldLines (file index)
  "Unfolds long lines that have been folded."
  (let ((line (nth index file))
	(output (remove (code-char 10) (nth index file)))
	(unfolded nil))
    (loop while (not unfolded)
	  do
	     (cond ((search (string (code-char 13)) line)
		    (setf index (+ index 1))
		    (setf line (nth index file))
		    (cond ((string= (char line 0) " ")
			   ;; CR followed by white space means this line is folded and needs to be unfold
			   (setf output (concatenate 'string output (remove (code-char 10) (remove (code-char 13) line)))))
			  (t (setf unfolded t))))
		   (t (setf unfolded t))))
    output))
		  

(defun createListFromFile (file)
  "Creates a list from the file."
  (let ((fileContent nil))
	(with-open-file (stream file)
	  (loop for line = (read-line stream nil nil) for index from 0
	    while line
	    do
	       (setf fileContent (append fileContent (list line)))))
	fileContent))

(defun createEventList (fileContent)
  "Creates a list of events from the file."
  (let ((eventList nil)
	(inVEVENT nil)
	(startTime nil)
	(endTime nil)
	(summary nil)
	(desc nil)
	(output nil))
	(loop for line in fileContent for index from 0
	  do
	     (cond ((= 0 index)
		    ;; RFC 5545 Chapter 3.4
		    ;; "[...] The first line and last line of the iCalendar object MUST contain a pair of iCalendar object delimiter strings."
		    (checkICS line))
		   ((and (checkForBEGINEVENT line) (not inVEVENT))
		    (setf inVEVENT t))
		   ((and (checkForStart line) inVEVENT)
		    (setf startTime (getStartTime line)))
		   ((and (checkForEndOrDuration line) inVEVENT)
		    (setf endTime (getEndTime startTime line)))
		   ((and (checkForSummary line) inVEVENT)
		    (setf summary (getSummaryDesc fileContent index)))
		   ((and (checkForDesc line) inVEVENT)
		    (setf desc (getSummaryDesc fileContent index)))
		   ((and (checkForENDEVENT line) inVEVENT)
		    (cond ((not startTime)
			   (format t "Found an event with missing start time ending at line ~a, skipping import.~%" index))
			  ((not endTime)
			   (format t "Found an event with missing end time ending at line ~a, skipping import.~%" index))
			  ((not summary)
			   (format t "Found an event with missing summary ending at line ~a, skipping import.~%" index))
			  (t
			   (with-standard-io-syntax
			     (if desc
				 (setf output (append output (list (list startTime endTime summary desc))))
				 (setf output (append output (list (list startTime endTime summary))))))))
		    (setf inVEVENT nil)
		    (setf startTime nil)
		    (setf endTime nil)
		    (setf summary nil)
		    (setf desc nil))))
	output))
