(in-package :lisp-ical-cli)
	    
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
  (let ((file (clingon:getopt cmd :file)))
    (format t "Importing file ~a~%" file)
    (with-open-file (stream file)
      (loop for line = (read-line stream nil nil) for index from 0
	    while line
	    do (format t "~a~%" line)
	    do (if (= 0 index)
		   (checkICS line))))))

(defun import/command ()
    (clingon:make-command
     :name "import"
     :description "Import an ical file into the database"
     :options (import/options)
     :handler #'import/handler))

(defun checkICS (line)
  (if (search "BEGIN:VCALENDAR" line)
      (progn
	(format t "File is ICS, CONTINUE~%"))
      (progn
	(format t "File is not ICS, EXIT~%")
	(SB-EXT:QUIT))))
