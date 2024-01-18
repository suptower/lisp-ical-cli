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
  (let ((event-list (make-event-list (make-list-from-file (clingon:getopt cmd :file)))))
    (format t "Inspecting events from file ~a.~%" (clingon:getopt cmd :file))
    (format t "Parsed all events from file, inspecting events now.~%")
    (iterateEvents event-list)))

(defun inspect/command ()
  (clingon:make-command
   :name "inspect"
   :description "Inspect an ical file for its events"
   :examples '(("Inspect Standup.ics file:" . "ical-cli inspect -f Standup.ics"))
   :options (inspect/options)
   :handler #'inspect/handler))

(defun iterateEvents (event-list)
  (let ((next-action 0)
	(index 0))
    (cond (event-list
	   (format t "Iterating through ~a events from file.~%~%" (length event-list))
	   (loop while (/= next-action 5)
		 do
		    (let ((start-time (nth 0 (nth index event-list)))
			  (end-time (nth 1 (nth index event-list)))
			  (summary (nth 2 (nth index event-list)))
			  (desc (nth 3 (nth index event-list))))
		      (format t "Showing event ~a from ~a.~%" (+ index 1) (length event-list))
		      (format t "~a - ~a: ~a~%" start-time end-time (display-sum-or-desc summary))
		      (setf next-action (make-next-action (nth index event-list) (+ index 1) (length event-list)))
		      (cond ((= next-action 1)
			     (format t "Event description: ~a~2&" (display-sum-or-desc desc)))
			    ((= next-action 2)
			     (format t "Importing event into database.~%")
			     (add-event (prepareForImport (nth index event-list)))
			     (format t "Imported event (start, end, summary): ~a, ~a, ~a~%~%" start-time end-time (display-sum-or-desc summary)))
			    ((= next-action 3)
			     (format t "Showing previous event.~%~%")
			     (setf index (- index 1)))
			    ((= next-action 4)
			     (format t "Showing next event.~%~%")
			     (setf index (+ index 1)))
			    (t
			     (setf next-action 5))))))
	   (t
	    (format t "No event in file was found.~%")))))


(defun make-next-action (event index length)
  "next action: 1 - show desc, 2 - import, 3 - prev, 4 - next, 5 - quit"
  (let ((desc (nth 3 event))
	(question "Next action to take")
	(answer nil)
	(options nil)
	(next-action nil))
    (if desc
	(setf options (append options (list (format nil "(d) show description")))))
    (setf options (append options (list (format nil "(i) import event into database"))))
    (if (> index 1)
	(setf options (append options (list (format nil "(p) show previous event")))))
    (if (< index length)
	(setf options (append options (list (format nil "(n) show next event")))))
    (setf options (append options (list (format nil "(q) quit"))))
    (loop while (not next-action)
	  do
	     (format t "~a:~%[~{~a~^/~}]~%" question options)
	     (setf answer (read-line *standard-input* nil (string "q")))
	     (cond ((and (search "d" answer) desc)
		    (setf next-action 1))
		   ((search "i" answer)
		    (setf next-action 2))
		   ((and (search "p" answer) (> index 1))
		    (setf next-action 3))
		   ((and (search "n" answer) (< index length))
		    (setf next-action 4))
		   ((search "q" answer)
		    (format t "Quitting...~%")
		    (setf next-action 5))
		   (t
		    (format t "You did not chose a correct option.~%"))))
    next-action))
		  
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
