(in-package :lisp-ical-cli)

(defun top-level/command ()
  (clingon:make-command
   :name "ical-cli"
   :version "0.0.1"
   :description "A command line interface for iCal files"
   :authors '("Arda Köcer <arda.koecer@st.oth-regensburg.de>")
   :handler #'top-level/handler
   :sub-commands (top-level/sub-commands)))

(defun top-level/sub-commands ()
  (list
   (import/command)))

(defun top-level/handler (cmd)
  (clingon:print-usage-and-exit cmd t))

(defun main ()
  (let ((app (top-level/command)))
    (clingon:run app)))
