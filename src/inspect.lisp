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
  (let ((file (clingon:getopt cmd :file)))))

(defun inspect/command ()
  (clingon:make-command
   :name "inspect"
   :description "Inspect an ical file for its events"
   :examples '(("Inspect Standup.ics file:" . "ical-cli inspect -f Standup.ics"))
   :options (inspect/options)
   :handler #'inspect/handler))
