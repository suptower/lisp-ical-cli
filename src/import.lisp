(in-package :ical-cli)

;; when importing a file, we need to check, if it is a true ICS file (has a VCALENDAR and VEVENT property)

(defun import/options ()
    (list
    (clingon:make-option
     :filepath
     :short-name #\f
     :long-name "file"
     :description "The ical file to import"
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
    (format t "Importing file ~a~%" file)
    (with-open-file (stream file)
      (loop for line = (read-line stream nil nil) for index from 0
	    while line
	    do (progn
		 (if (= 0 index)
		     (checkICS line))
		 (if (and (checkForVEVENT line)
			  (not inVEVENT))
		     (progn
		       (format t "Found BEGIN:VEVENT at index ~a~%" index)
		       (setf inVEVENT t)))
		 (if (and (checkForStart line) inVEVENT)
		     (progn
		       (format t "Found DTSTART at index ~a~%" index)
		       (setf startTimeFound t)
		       (setf startTime (getStartTime line))
		       (format t "starttime: ~a~%" startTime)))
		 (if (and (checkForEndOrDuration line) inVEVENT)
		     (progn
		       (format t "Found DTEND/DURATION at index ~a~%" index)
		       (setf endTimeFound t)
		       (setf endTime (getEndTime startTime line))
		       (format t "endtime: ~a~%" endTime)))
		 (if (and (checkForSummary line) inVEVENT)
		     (progn
		       (format t "Found SUMMARY at index ~a~%" index)
		       (setf summaryFound t)
		       (setf summary (getSummary line))
		       (format t "summary: ~a~%" summary))))))
    (if (and inVEVENT startTimeFound endTimeFound summaryFound)
	(progn
	  (setf output (format nil "~a,~a,~a" startTime endTime summary))
	  (addEvent output))
	(format t "ICS file is either missing start time, end time or summary."))))

(defun import/command ()
    (clingon:make-command
     :name "import"
     :description "Import an ical file into the database"
     :options (import/options)
     :handler #'import/handler))

(defun checkICS (line)
  (if (search "BEGIN:VCALENDAR" line)
      (progn
	(format t "File passed ICS check, program will be continued.~%"))
      (progn
	(format t "The specified file to import is not a true ICS file, exiting program.~%")
	(SB-EXT:QUIT))))
