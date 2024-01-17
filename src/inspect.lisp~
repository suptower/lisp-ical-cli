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
  (let ((eventList (createEventList (createListFromFile (clingon:getopt cmd :file)))))
    (format t "Inspecting events from file ~a.~%" (clingon:getopt cmd :file))
    (format t "Parsed all events from file, inspecting events now.~%")
    (iterateEvents eventList)))

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
