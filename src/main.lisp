(in-package :cl-user)
(defpackage :lisp-ical-cli
  (:use :cl)
  (:import-from :clingon)
  (:export :main))
(in-package :lisp-ical-cli)

(defun import-ical/handler (file)
  (let ((stream (open file)))
    (loop for line = (read-line stream nil nil)
          while line
          do (format t "~A~%" line))
    (close stream))
  (values))

(defun import-ical/command ()
  (clingon:make-command
   :name "import-ical"
   :description "Import an iCal file"))

(defun top-level/command ()
  (clingon:make-command
   :name "ical-cli"
   :version "0.0.1"
   :description "A command line interface for iCal files"
   :authors '("Arda Köcer <arda.koecer@st.oth-regensburg.de>")
   :usage "[-f <file>]"
   :options (top-level/options)
   :handler #'top-level/handler))

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
