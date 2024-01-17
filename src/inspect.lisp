(in-package :ical-cli)

(defun inspect/options ()
  (list
   (clingon:make-option
    :filepath
    :short-name #\f
    :long-name "file"
    :description "(REQUIRED) The ical file to inspect"
    :key :file
    :required t)))

(defun inspect/handler (cmd)
  (let ((file (clingon:getopt cmd :file))
	(fileContent nil)
	(inVEVENT nil)
	(startTime nil)
	(endTime nil)
	(summary nil)
	(desc nil)
	(output nil))
    (format t "Inspecting events from file ~a.~%" file)
    (with-open-file (stream file)
      (loop for line = (read-line stream nil nil) for index from 0
	    while line
	    do
	       (setf fileContent (append fileContent (list line)))))
    (loop for line in fileContent for index from 0
	  do
	     (cond ((= 0 index)
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
			   (format t "Found an event with missing start time ending at line ~a, skipping event.~%" index))
			  ((not endTime)
			   (format t "Found an event with missing end time ending at line ~a, skipping event.~%" index))
			  ((not summary)
			   (format t "Found an event with missing summary ending at line ~a, skipping event.~%" index))
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
    (format t "Parsed all events from file, inspecting events now.~%")
    (iterateEvents output)))

(defun inspect/command ()
  (clingon:make-command
   :name "inspect"
   :description "Inspect an ical file for its events"
   :examples '(("Inspect Standup.ics file:" . "ical-cli inspect -f Standup.ics"))
   :options (inspect/options)
   :handler #'inspect/handler))

(defun iterateEvents (eventList)
  (let ((nextAction 0)
	(index 0))
    (cond (eventList
	   (format t "Iterating through ~a events from file.~%~%" (length eventList))
	   (loop while (/= nextAction 5)
		 do
		    (let ((startTime (nth 0 (nth index eventList)))
			  (endTime (nth 1 (nth index eventList)))
			  (summary (nth 2 (nth index eventList)))
			  (desc (nth 3 (nth index eventList)))
			  (imported nil))
		      (format t "Showing event ~a from ~a.~%" (+ index 1) (length eventList))
		      (format t "~a - ~a: ~a~%" startTime endTime (displaySumOrDesc summary))
		      (setf nextAction (getNextAction (nth index eventList) imported (+ index 1) (length eventList)))
		      (cond ((= nextAction 1)
			     (format t "Event description: ~a~%~%" (displaySumOrDesc desc)))
			    ((= nextAction 2)
			     (format t "Importing event into database.~%")
			     (addEvent (prepareForImport (nth index eventList)))
			     (setf imported t)
			     (format t "Imported event (start, end, summary): ~a, ~a, ~a~%~%" startTime endTime (displaySumOrDesc summary)))
			    ((= nextAction 3)
			     (format t "Showing previous event.~%~%")
			     (setf index (- index 1)))
			    ((= nextAction 4)
			     (format t "Showing next event.~%~%")
			     (setf index (+ index 1)))
			    (t
			     (setf nextAction 5))))))
	   (t
	    (format t "No event in file was found.~%")))))


(defun getNextAction (event imported index length)
  "next action: 1 - show desc, 2 - import, 3 - prev, 4 - next, 5 - quit"
  (let ((desc (nth 3 event))
	(question "Next action to take")
	(answer nil)
	(options nil)
	(nextAction nil))
    (if desc
	(setf options (append options (list (format nil "(d) show description")))))
    (if (not imported)
	(setf options (append options (list (format nil "(i) import event into database")))))
    (if (> index 1)
	(setf options (append options (list (format nil "(p) show previous event")))))
    (if (< index length)
	(setf options (append options (list (format nil "(n) show next event")))))
    (setf options (append options (list (format nil "(q) quit"))))
    (loop while (not nextAction)
	  do
	     (format t "~a:~%[~{~a~^/~}]~%" question options)
	     (setf answer (read-line))
	     (cond ((and (search "d" answer) desc)
		    (setf nextAction 1))
		   ((and (search "i" answer) (not imported))
		    (setf nextAction 2))
		   ((and (search "p" answer) (> index 1))
		    (setf nextAction 3))
		   ((and (search "n" answer) (< index length))
		    (setf nextAction 4))
		   ((search "q" answer)
		    (setf nextAction 5))
		   (t
		    (format t "You did not chose a correct option.~%"))))
    nextAction))
		  
(defun prepareForImport (event)
  (let ((start (nth 0 event))
	(end (nth 1 event))
	(sum (nth 2 event))
	(desc (nth 3 event))
	(output nil))
    (if desc
	(setf output (format nil "~$::~$::~$::~$" start end sum desc))
	(setf output (format nil "~$::~$::~$" start end sum)))
    output))
