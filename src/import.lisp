(in-package :ical-cli)

(defvar BEGIN-VEVENT-PROP "BEGIN:VEVENT")
(defvar END-VEVENT-PROP "END:VEVENT")
(defvar DTSTART-PROP "DTSTART")
(defvar DTEND-PROP "DTEND")
(defvar DURATION-PROP "DURATION")
(defvar SUMMARY-PROP "SUMMARY")
(defvar DESCRIPTION-PROP "DESCRIPTION")

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
  (let ((event-list (make-event-list (make-list-from-file (clingon:getopt cmd :file)))))
    (cleanup-database)
    (format t "Importing file ~a.~%" (clingon:getopt cmd :file))
	(format t "Found ~a events in file.~%" (length event-list))
	(loop for event in event-list do
	  (let ((start-time (nth 0 event))
		(end-time (nth 1 event))
		(summary (nth 2 event))
		(desc (nth 3 event)))
	    (format t "Imported event (start, end, summary): ~a, ~a, ~a~&" start-time end-time (display-sum-or-desc summary))
	    (cond ((not desc)
		   (add-event (format nil "~$::~$::~$" start-time end-time summary)))
		  (t
		   (add-event (format nil "~$::~$::~$::~$" start-time end-time summary desc))))))
    (format t "Import finished.~%")))

(defun import/command ()
    (clingon:make-command
     :name "import"
     :description "Import an ical file into the database"
     :examples '(("Import Standup.ics file:" . "ical-cli import -f Standup.ics"))
     :options (import/options)
     :handler #'import/handler))

(defun check-ICS (line)
  "Checks if the file is a true ICS file, if not, the program will exit."
  (if (search "BEGIN:VCALENDAR" line)
      (progn
	(format t "File passed ICS check, program will be continued.~%"))
      (progn
	(format t "The specified file to import is not a true ICS file, exiting program.~%")
	(SB-EXT:QUIT))))

(defun check-ical-property (line property)
	"Checks if the line contains the given property string."
	(if (search property line)
		t
		nil))

(defun make-start-time (line)
  "Gets the start time from the line and returns it as a local timestamp."
  (format-local-time (create-local-timestamp (subseq line (+ (position #\: line :test #'equal) 1)))))

(defun make-end-time (start-time line)
  "Gets the end time from the line (calculates it if necessary by using duration and start time) and returns it as a local timestamp."
  (cond ((check-ical-property line DTEND-PROP)
  		(format-local-time (create-local-timestamp (subseq line (+ (position #\: line :test #'equal) 1)))))
	((check-ical-property line DURATION-PROP)
		(format-local-time (calculate-end-time (create-local-from-hr start-time) (subseq line (+ (position #\: line :test #'equal) 1)))))
	(t (format t "FAILURE, NO DTEND OR DURATION FOUND."))))

(defun make-summary-or-desc (file index)
  "Gets the summary or description from the line, checks for continous lines, removes LF and CR, then returns it as a string."
  (let ((line (unfold-lines file index)))
    (remove (code-char 13) (remove (code-char 10) (subseq (subseq line (+ (position #\: line :test #'equal) 1)) 0 (length (subseq line (+ (position #\: line :test #'equal) 1))))))))

(defun unfold-lines (file index)
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
		  

(defun make-list-from-file (file)
  "Creates a list of lines from the file."
  (let ((file-content nil))
	(with-open-file (stream file)
	  (loop for line = (read-line stream nil nil) for index from 0
	    while line
	    do
	       (setf file-content (append file-content (list line)))))
	file-content))

(defun make-event-list (file-content)
  "Creates a list of events from the list of lines from the file."
  (let ((event-list nil)
	(in-VEVENT nil)
	(start-time nil)
	(end-time nil)
	(summary nil)
	(desc nil)
	(output nil))
	(loop for line in file-content for index from 0
	  do
	     (cond ((= 0 index)
		    ;; RFC 5545 Chapter 3.4
		    ;; "[...] The first line and last line of the iCalendar object MUST contain a pair of iCalendar object delimiter strings."
		    (check-ics line))
		   ((and (check-ical-property line BEGIN-VEVENT-PROP) (not in-VEVENT))
		    (setf in-VEVENT t))
		   ((and (check-ical-property line DTSTART-PROP) in-VEVENT)
		    (setf start-time (make-start-time line)))
		   ((and (or (check-ical-property line DTEND-PROP) (check-ical-property line DURATION-PROP)) in-VEVENT)
		    (setf end-time (make-end-time start-time line)))
		   ((and (check-ical-property line SUMMARY-PROP) in-VEVENT)
		    (setf summary (make-summary-or-desc file-content index)))
		   ((and (check-ical-property line DESCRIPTION-PROP) in-VEVENT)
		    (setf desc (make-summary-or-desc file-content index)))
		   ((and (check-ical-property line END-VEVENT-PROP) in-VEVENT)
		    (cond ((not start-time)
			   (format t "Found an event with missing start time ending at line ~a, skipping import.~%" index))
			  ((not end-time)
			   (format t "Found an event with missing end time ending at line ~a, skipping import.~%" index))
			  ((not summary)
			   (format t "Found an event with missing summary ending at line ~a, skipping import.~%" index))
			  (t
			   (with-standard-io-syntax
			     (if desc
				 (setf output (append output (list (list start-time end-time summary desc))))
				 (setf output (append output (list (list start-time end-time summary))))))))
		    (setf in-VEVENT nil)
		    (setf start-time nil)
		    (setf end-time nil)
		    (setf summary nil)
		    (setf desc nil))))
	output))