(in-package :ical-cli)

(defun inspect/options ()
  (list
   (clingon:make-option
    :filepath
    :short-name #\f
    :long-name "file"
    :description "(REQUIRED) The ical file to inspect"
    :key :file
    :required t)
    (clingon:make-option
    :flag
    :short-name #\d
    :long-name "detailed"
    :description "Show detailed description of events"
    :key :detail)))

(defun inspect/handler (cmd)
  (let ((file (clingon:getopt cmd :file))
        (detail (clingon:getopt cmd :detail))
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
    (format t "Inspecting events from file ~a.~%" file)
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
			       (format t "Found an event with missing start time ending at line ~a, skipping event.~%" index))
			      ((not endTimeFound)
			       (format t "Found an event with missing end time ending at line ~a, skipping event.~%" index))
			      ((not summaryFound)
			       (format t "Found an event with missing summary ending at line ~a, skipping event.~%" index))
			      (t
			       (with-standard-io-syntax
				 (if descFound
				     (setf output (append output (list (list startTime endTime summary desc))))
				     (setf output (append output (list (list startTime endTime summary))))))))
			(setf inVEVENT nil)
			(setf startTimeFound nil)
			(setf endTimeFound nil)
			(setf summaryFound nil)
			(setf descFound nil))))))
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
  (cond (eventList
	 (format t "Iterating through ~a events from file.~%" (length eventList))
	 (loop for event in eventList for index from 1
	       do
		  (let ((startTime (nth 0 event))
			(endTime (nth 1 event))
			(summary (nth 2 event))
			(desc (nth 3 event))
			(nextAction nil))
		    (format t "Showing event ~a from ~a.~%" index (length eventList))
		    (format t "~a: ~a~%" (getTimes startTime endTime) summary)
		    (setf nextAction (getNextAction event index (length eventList)))
		    (format t "Next action: ~a~%" nextAction))))
	(t
	 (format t "No event in file was found.~%"))))


(defun getNextAction (event index length)
  "next action: 1 - show desc, 2 - import, 3 - prev, 4 - next, 5 - quit"
  (let ((desc (nth 3 event))
	(question "Next action to take")
	(answer nil)
	(options nil)
	(nextAction nil))
    (if desc
	(setf options (append options (list (format nil "(d) show description")))))
    (setf options (append options (list (format nil "(i) import event into database"))))
    (if (> index 1)
	(setf options (append options (list (format nil "(p) show previous event")))))
    (if (< index length)
	(setf options (append options (list (format nil "(n) show next event")))))
    (setf options (append options (list (format nil "(q) quit"))))
    (format t "~a:~%[ ~{~a~^ / ~} ]~%" question options)
    (loop while (not nextAction)
	   do
	   (setf answer (read-line))
	   (cond ((and (search "d" answer) desc)
		  (setf nextAction 1))
		 ((search "i" answer)
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
		  
