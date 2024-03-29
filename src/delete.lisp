;;;; delete.lisp
;;;;
;;;; This file containt the delete command and handles
;;;; deleting all events in the database as well as returning an empty one as result.

(in-package :ical-cli)

(defun delete/options ()
  (list
   (clingon:make-option
    :flag
    :description "Force-skip prompt"
    :short-name #\f
    :long-name "force"
    :key :skip)))

(defun delete/handler (cmd)
  "Delete whole event database"
  (let ((skip (clingon:getopt cmd :skip)))
    (if (not skip)
	(progn
	  (finish-output *query-io*)
	  (format t "This will delete all events from the database.~%Are you sure? [yes/no]:~%")
	  (setf skip (read-line))
	  (if (or (search "yes" skip) (string= "y" skip))
	      (setf skip t)
	      (setf skip nil))))
    (if skip
	(progn
	  (if (probe-file "~/.event_database")
	      (progn
		(delete-file "~/.event_database")
		(open "~/.event_database" :direction :probe :if-does-not-exist :create)
		(format t "Deleted all events.~%")))))))

(defun delete/command ()
  "Creates and returns the delete command"
  (clingon:make-command
   :name "delete"
   :description "Clear whole event database"
   :version "0.1.0"
   :options (delete/options)
   :handler #'delete/handler))
