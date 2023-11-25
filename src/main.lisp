(in-package :lisp-ical-cli)

(defun top-level/command ()
  (clingon:make-command
   :name "ical-cli"
   :version "0.0.1"
   :description "A command line interface for iCal files"
   :authors '("Arda Köcer <arda.koecer@st.oth-regensburg.de>")
   :usage "[-f <file>]"
   :options (top-level/options)
   :handler #'top-level/handler
   :sub-commands (top-level/sub-commands)))

(defun top-level/sub-commands ()
  (list
   (import/command)))

(defun top-level/options ()
  (list
   (clingon:make-option
    :string
    :short-name #\f
    :long-name "file"
    :description "The iCal file to import"
    :required t
    :key :file)))

(defun top-level/handler (cmd)
  (let ((args (clingon:command-arguments cmd))
        (file (clingon:getopt cmd :file)))
        (format t "Importing ~A~%" file)))

(defun main ()
  (let ((app (top-level/command)))
    (clingon:run app)))
