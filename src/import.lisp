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
	(inVEVENT nil))
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
		       (format t "~a~%" line)
		       (setf inVEVENT t)))
		 (if (and (checkForStart line) inVEVENT)
		     (progn
		       (format t "Found DTSTART at index ~a~%" index)
		       (format t "~a~%" (getStartTime line))))
		 (if (and (checkForEndOrDuration line) inVEVENT)
		     (progn
		       (format t "Found DTEND/DURATION at index ~a~%" index)
		       (format t "~a~%" (getEndTime line)))))))))		 

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
